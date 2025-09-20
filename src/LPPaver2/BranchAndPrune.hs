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
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.List qualified as List
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints
import MixedTypesNumPrelude
import Text.Printf (printf)

type LPPProblem = BP.Problem Form Box

type LPPPaving = BP.Paving Form Box Boxes

type LPPStep = BP.Step LPPProblem LPPPaving

type LPPBPResult = BP.Result Form Box Boxes

data LPPBPParams = LPPBPParams
  { problem :: LPPProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBPLPPProblem :: Rational -> LPPProblem -> Bool
shouldGiveUpOnBPLPPProblem giveUpAccuracy (BP.Problem {scope = Box {..}}) =
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
    BP.CanControlSteps m LPPStep
  ) =>
  r ->
  LPPBPParams ->
  m LPPBPResult
lppBranchAndPrune sampleR (LPPBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBPLPPProblem giveUpAccuracy :: LPPProblem -> Bool,
          BP.dummyPriorityQueue,
          BP.maxThreads,
          BP.shouldLog
        }
    )
  where
    dummyPriorityQueue :: BoxStack
    dummyPriorityQueue = BoxStack [problem]

instance
  (CanEval r, HasKleenanComparison r, Applicative m) =>
  BP.CanPrune m r Form Box Boxes
  where
  pruneProblemM sampleR (BP.Problem {scope, constraint}) = pure pavingP
    where
      result = simplifyEvalForm sampleR scope constraint
      simplifiedConstraint = result.evaluatedForm.form
      pavingP = case getFormDecision simplifiedConstraint of
        CertainTrue -> BP.pavingInner scope (Boxes [scope])
        CertainFalse -> BP.pavingOuter scope (Boxes [scope])
        _ -> BP.pavingUndecided scope [BP.Problem {scope, constraint = simplifiedConstraint}]

newtype BoxStack = BoxStack [LPPProblem]

instance BP.IsPriorityQueue BoxStack LPPProblem where
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

instance BP.ShowStats (BP.Subset Boxes Box) where
  showStats (BP.Subset {..}) =
    printf "{|boxes| = %d, coverage = %3.4f%%}" (boxesCount subset) coveragePercent
    where
      coveragePercent = 100 * (boxesAreaD subset / boxAreaD superset)

instance BP.IsSet Boxes where
  emptySet = Boxes []
  setIsEmpty (Boxes boxes) = null boxes
  setIsEmpty (BoxesUnion union) = List.all BP.setIsEmpty union
  setUnion bs1 bs2 = BoxesUnion [bs1, bs2]

instance BP.BasicSetsToSet Box Boxes where
  basicSetsToSet = Boxes

instance BP.CanSplitProblem constraint Box where
  splitProblem (BP.Problem {scope, constraint}) =
    map (\box -> BP.Problem {scope = box, constraint}) $ splitBox scope
