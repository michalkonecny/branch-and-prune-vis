module App.StepsReader
  ( component
  , Output(..)
  , Query
  , Slot(..)
  , StepsState(..)
  , initialState
  ) where

import Prelude

import App.Steps (Problem, ProblemHash, Step(..), getStepParent, getStepProblems, parseSteps)
import App.Utils (actOnStateUntil)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS

type StepsState =
  { steps :: Map ProblemHash Step
  , problems :: Map ProblemHash Problem
  , initProblem :: Maybe Problem
  , completion :: Maybe String
  }

data Output = OutputNewStepsState StepsState

data Query :: Type -> Type
data Query a = Void

type Slot id = H.Slot Query Output id

initialState :: forall input. input -> StepsState
initialState _ =
  { steps: Map.empty
  , problems: Map.empty
  , initProblem: Nothing
  , completion: Nothing
  }

component :: forall q i m. (MonadAff m) => H.Component q i Output m
component =
  H.mkComponent
    { initialState
    , render: \_ -> HH.div_ [] -- this is an invisible component, integrated using an empty HTML element
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

data Action = Initialize | NewSteps (Array Step)

handleAction :: forall cs m. (MonadAff m) => Action -> H.HalogenM StepsState Action cs Output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< stepsEmitter
    pure unit
  NewSteps steps -> do 
    H.modify_ (\st -> Array.foldl (flip addStep) st steps)
    st <- H.get
    H.raise (OutputNewStepsState st)

addStep :: Step -> StepsState -> StepsState
addStep step st =
  st
    { steps = steps
    , problems = problems
    , initProblem = initProblem
    , completion = completion
    }
  where
  steps = case getStepParent step of
    Nothing -> st.steps
    Just parent -> Map.insert parent step st.steps
  problems = Array.foldl addProblem st.problems (getStepProblems step)
  addProblem probs problem = Map.insert problem.contentHash problem probs
  initProblem = case step of
    InitStep { problem } -> Just problem
    _ -> st.initProblem
  completion = case step of
    DoneStep -> Just "done"
    AbortStep { detail } -> Just detail
    _ -> st.completion

stepsEmitter :: forall m. MonadAff m => m (HS.Emitter Action)
stepsEmitter = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ actOnStateUntil
    { initState: { howManyReadSoFar: 0, done: false }
    , action: dealWithNewStepsAndWaitABit listener
    , shouldFinish: \s -> s.done
    }
  pure emitter
  where
  dealWithNewStepsAndWaitABit listener { howManyReadSoFar } = do
    steps <- getNewSteps howManyReadSoFar
    Aff.delay $ Aff.Milliseconds 10.0 -- without this the first step is emitted before subscription takes effect
    _ <- H.liftEffect $ HS.notify listener $ NewSteps steps
    Aff.delay $ Aff.Milliseconds 100.0
    pure
      { howManyReadSoFar: howManyReadSoFar + (Array.length steps)
      , done: Array.any isDoneStep steps
      }

isDoneStep :: Step -> Boolean
isDoneStep DoneStep = true
isDoneStep _ = false

foreign import _getNewSteps :: Int -> Effect (Promise (Array String))

getNewSteps :: Int -> Aff (Array Step)
getNewSteps currLenghtRead = do
  stepTexts <- Promise.toAffE (_getNewSteps currLenghtRead)
  pure $ parseSteps stepTexts
