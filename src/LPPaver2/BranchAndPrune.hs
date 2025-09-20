{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    LPPProblem,
    LPPPaving,
    LPPStep,
    LPPBPResult,
    LPPBPParams (..),
    lppBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as AbstrBP
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints.Boxes
  ( Box (..),
    Boxes (..),
    boxAreaD,
    boxesAreaD,
    boxesCount,
    splitBox,
  )
import LPPaver2.RealConstraints.Eval
  ( CanEval,
    EvaluatedForm (..),
    HasKleenanComparison,
    SimplifyFormResult (..),
    simplifyEvalForm,
  )
import LPPaver2.RealConstraints.Form (Form, getFormDecision)
import MixedTypesNumPrelude
import Text.Printf (printf)

type LPPProblem = AbstrBP.Problem Form Box

type LPPPaving = AbstrBP.Paving Form Box Boxes

type LPPStep = AbstrBP.Step LPPProblem LPPPaving

type LPPBPResult = AbstrBP.Result Form Box Boxes

data LPPBPParams = LPPBPParams
  { problem :: LPPProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBPLPPProblem :: Rational -> LPPProblem -> Bool
shouldGiveUpOnBPLPPProblem giveUpAccuracy (AbstrBP.Problem {scope = Box {..}}) =
  all smallerThanPrec (Map.elems varDomains)
  where
    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

lppBranchAndPrune ::
  ( MonadLogger m,
    MonadUnliftIO m,
    CanEval r,
    HasKleenanComparison r,
    AbstrBP.CanControlSteps m LPPStep
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune sampleR (LPPBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  AbstrBP.branchAndPruneM
    ( AbstrBP.Params
        { AbstrBP.problem,
          AbstrBP.pruningMethod = sampleR,
          AbstrBP.shouldAbort = const Nothing,
          AbstrBP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          AbstrBP.dummyPriorityQueue,
          AbstrBP.maxThreads,
          AbstrBP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

instance
  (CanEval r, HasKleenanComparison r, Applicative m) =>
  AbstrBP.CanPrune m r Form Box Boxes
  where
  pruneProblemM sampleR (AbstrBP.Problem {scope, constraint}) = pure pavingP
    where
      result = simplifyEvalForm sampleR scope constraint
      simplifiedConstraint = result.evaluatedForm.form
      pavingP = case getFormDecision simplifiedConstraint of
        CertainTrue -> AbstrBP.pavingInner scope (Boxes [scope])
        CertainFalse -> AbstrBP.pavingOuter scope (Boxes [scope])
        _ -> AbstrBP.pavingUndecided scope [AbstrBP.Problem {scope, constraint = simplifiedConstraint}]

newtype BoxStack = BoxStack [LPPProblem]

instance AbstrBP.IsPriorityQueue BoxStack LPPProblem where
  singletonQueue e = BoxStack [e]
  queueToList (BoxStack list) = list
  queuePickNext (BoxStack []) = Nothing
  queuePickNext (BoxStack (e : es)) = Just (e, BoxStack es)
  queueAddMany (BoxStack es) new_es = BoxStack (new_es ++ es)
  queueSplit (BoxStack es)
    | splitPoint == 0 = Nothing
    | otherwise = Just (BoxStack esL, BoxStack esR)
    where
      splitPoint = length es `divI` 2
      (esL, esR) = splitAt splitPoint es

  queueMerge (BoxStack stackL) (BoxStack stackR) = BoxStack $ stackL ++ stackR

instance AbstrBP.ShowStats (AbstrBP.Subset Boxes Box) where
  showStats (AbstrBP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance AbstrBP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setIsEmpty (BoxesUnion union) = List.all AbstrBP.setIsEmpty union
  setUnion bs1 bs2 = BoxesUnion [bs1, bs2]

instance AbstrBP.BasicSetsToSet Box Boxes where
  basicSetsToSet = Boxes

instance AbstrBP.CanSplitProblem constraint Box where
  splitProblem (AbstrBP.Problem {scope, constraint}) =
    map (\box -> AbstrBP.Problem {scope = box, constraint}) $ splitBox scope
