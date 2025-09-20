{-# LANGUAGE UndecidableInstances #-}

module LPPaver2.BranchAndPrune
  ( --
    BoxProblem,
    BoxPaving,
    BoxStep,
    BoxResult,
    BoxBPParams (..),
    boxBranchAndPrune,
  )
where

import AERN2.MP (Kleenean (..), MPBall)
import AERN2.MP qualified as MP
import BranchAndPrune.BranchAndPrune qualified as BP
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Map qualified as Map
import GHC.Records
import LPPaver2.RealConstraints.Boxes (Box (..), Boxes (..))
import LPPaver2.RealConstraints.Eval (CanEval, EvaluatedForm (..), HasKleenanComparison, SimplifyFormResult (..), simplifyEvalForm)
import LPPaver2.RealConstraints.Form (Form, getFormDecision)
import MixedTypesNumPrelude

type BoxProblem = BP.Problem Form Box

type BoxPaving = BP.Paving Form Box Boxes

type BoxStep = BP.Step BoxProblem BoxPaving

type BoxResult = BP.Result Form Box Boxes

data BoxBPParams = BoxBPParams
  { problem :: BoxProblem,
    maxThreads :: Int,
    giveUpAccuracy :: Rational,
    shouldLog :: Bool
  }

shouldGiveUpOnBoxProblem :: Rational -> BoxProblem -> Bool
shouldGiveUpOnBoxProblem giveUpAccuracy (BP.Problem {scope = Box {..}}) =
  all smallerThanPrec (Map.elems varDomains)
  where
    smallerThanPrec :: MPBall -> Bool
    smallerThanPrec ball = diameter <= giveUpAccuracy
      where
        diameter = 2 * MP.radius ball

boxBranchAndPrune ::
  ( MonadLogger m,
    MonadUnliftIO m,
    CanEval r,
    HasKleenanComparison r,
    BP.CanControlSteps m BoxStep
  ) =>
  r ->
  BoxBPParams ->
  m BoxResult
boxBranchAndPrune sampleR (BoxBPParams {..}) = do
  -- conn <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  BP.branchAndPruneM
    ( BP.Params
        { BP.problem,
          BP.pruningMethod = sampleR,
          BP.shouldAbort = const Nothing,
          BP.shouldGiveUpSolvingProblem = shouldGiveUpOnBoxProblem giveUpAccuracy :: BoxProblem -> Bool,
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

newtype BoxStack = BoxStack [BoxProblem]

instance BP.IsPriorityQueue BoxStack BoxProblem where
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
