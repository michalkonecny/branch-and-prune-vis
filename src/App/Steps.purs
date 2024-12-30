module App.Steps
  ( Box
  , Boxes(..)
  , Interval
  , Paving
  , Problem
  , ProblemHash
  , Step'
  , Step(..)
  , Var
  , fromStep'
  , getStepParent
  , getStepProblems
  , getStepsProblems
  , parseSteps
  , showStepEssence
  ) where

import Prelude

import Control.Monad.Except (except)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, E, readImpl, readJSON)
import Yoga.JSON.Generics.TaggedSumRep (genericReadForeignTaggedSum)
import Yoga.JSON.Generics.TaggedSumRep as TaggedSumRep

type Var = String

type Interval = { l :: Number, u :: Number }

type Box =
  { varDomains :: Map Var Interval
  , splitOrder :: Array Var
  }

dummyBox :: Box
dummyBox = { varDomains: Map.empty, splitOrder: [] }

data Boxes = Boxes (Array Box)

derive instance Generic Boxes _

instance ReadForeign Boxes
  where
  readImpl = genericReadForeignTaggedSum options
    where
    options = TaggedSumRep.defaultOptions { typeTag = "tag", valueTag = "contents" }

type ProblemHash = String

type Problem =
  { scope :: Box
  , constraint :: String
  , contentHash :: ProblemHash
  }

dummyProblem :: Problem
dummyProblem = { scope: dummyBox, constraint: "", contentHash: "" }

type Paving =
  { scope :: Box
  , inner :: Boxes
  , undecided :: Array Problem
  , outer :: Boxes
  }

dummyPaving :: Paving
dummyPaving = { scope: dummyBox, inner: Boxes [], undecided: [], outer: Boxes [] }

data Step
  = InitStep
      { problem :: Problem
      }
  | PruneStep
      { problemHash :: ProblemHash
      , prunePaving :: Paving
      }
  | SplitStep
      { problemHash :: ProblemHash
      , pieces :: Array Problem
      }
  | GiveUpOnProblemStep
      { problemHash :: ProblemHash
      }
  | AbortStep
      { detail :: String
      }
  | DoneStep

derive instance Generic Step _

type Step' =
  { tag :: String
  , problem :: Maybe Problem
  , problemHash :: Maybe ProblemHash
  , prunePaving :: Maybe Paving
  , pieces :: Maybe (Array Problem)
  , detail :: Maybe String
  }

fromStep' :: Step' -> E Step
fromStep'
  { tag
  , problem: maybeProblem
  , problemHash: maybeProlbemHash
  , prunePaving: maybePrunePaving
  , pieces: maybePieces
  , detail: maybeDetail
  } =
  let
    problemHash = fromJustWithDefault "BAD PROBLEM HASH" maybeProlbemHash
    problem = fromJustWithDefault dummyProblem maybeProblem
    prunePaving = fromJustWithDefault dummyPaving maybePrunePaving
    pieces = fromJustWithDefault [] maybePieces
    detail = fromJustWithDefault "BAD DETAIL" maybeDetail
  in
    case tag of
      "InitStep" -> Right $ InitStep { problem }
      "PruneStep" -> Right $ PruneStep { problemHash, prunePaving }
      "SplitStep" -> Right $ SplitStep { problemHash, pieces }
      "GiveUpOnProblemStep" -> Right $ GiveUpOnProblemStep { problemHash }
      "AbortStep" -> Right $ AbortStep { detail }
      "DoneStep" -> Right $ DoneStep
      _ -> Left (NonEmptyList $ ForeignError "Unrecognised step JSON" :| Nil)

fromJustWithDefault :: forall t. t -> Maybe t -> t
fromJustWithDefault _ (Just value) = value
fromJustWithDefault def _ = def

instance ReadForeign Step
  where
  readImpl f = do
    step' <- readImpl f
    case fromStep' step' of
      Right step -> pure step
      Left err -> except $ Left err

showStepEssence :: Step -> String
showStepEssence (InitStep _) = "InitStep"
showStepEssence (PruneStep { prunePaving: { inner: (Boxes inner), outer: (Boxes outer) } }) = "PruneStep (" <> showInner inner <> showOuter outer <> ")"
showStepEssence (SplitStep _) = "SplitStep"
showStepEssence (GiveUpOnProblemStep _) = "GiveUpOnProblemStep "
showStepEssence (AbortStep { detail }) = "AbortStep " <> detail
showStepEssence (DoneStep) = "DoneStep"

showInner :: forall t. Array t -> String
showInner inner =
  let
    n = Array.length inner
  in
    if n > 0 then (show n) <> " inner " else ""

showOuter :: forall t. Array t -> String
showOuter outer =
  let
    n = Array.length outer
  in
    if n > 0 then (show n) <> " outer " else ""

getStepProblems :: Step -> Array Problem
getStepProblems (InitStep { problem }) = [ problem ]
getStepProblems (PruneStep { prunePaving }) = prunePaving.undecided
getStepProblems (SplitStep { pieces }) = pieces
getStepProblems (GiveUpOnProblemStep _) = []
getStepProblems (AbortStep _) = []
getStepProblems (DoneStep) = []

getStepsProblems :: Array Step -> Array Problem
getStepsProblems = Array.concat <<< map getStepProblems

getStepParent :: Step -> Maybe ProblemHash
getStepParent (InitStep {}) = Nothing
getStepParent (PruneStep { problemHash }) = Just problemHash
getStepParent (SplitStep { problemHash }) = Just problemHash
getStepParent (GiveUpOnProblemStep { problemHash }) = Just problemHash
getStepParent (AbortStep _) = Nothing
getStepParent (DoneStep) = Nothing

parseSteps :: Array String -> Array Step
parseSteps stepJSONs =
  Array.catMaybes $ map tryParse stepJSONs
  where
  tryParse s =
    case readJSON s of
      Left _ -> Nothing
      Right step -> Just step