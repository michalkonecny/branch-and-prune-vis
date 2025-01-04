module App.ListSteps where

import App.Steps
import Prelude

import App.Utils (actOnStateUntil)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

type State = { steps :: Map ProblemHash Step, problem :: Maybe Problem, completion :: Maybe String }

initialState :: _ -> State
initialState _ = { steps: Map.empty, problem: Nothing, completion: Nothing }

data Action = Initialize | NewStep Step

component :: forall q i o m. (MonadAff m) => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ renderStepsAsTree state.problem state.steps ]

renderStepsAsTree :: Maybe Problem -> Map ProblemHash Step -> _
renderStepsAsTree Nothing _ = HH.text "No steps"
renderStepsAsTree (Just problem) steps =
  renderProblem problem.contentHash
  where
  renderProblem problemHash =
    HH.table
      [ HP.style "border: 1px solid;" ]
      [ HH.tbody_ $
          [ HH.tr_ [ HH.td [ HP.colSpan 2 ] [ HH.text $ showStepEssence step ] ] ]
            <>
              (map renderSubProblem $ getStepProblems step)
      ]
    where
    step = case Map.lookup problemHash steps of
      Just step_ -> step_
      _ -> AbortStep { detail: "No step with hash " <> problemHash }

    renderSubProblem p =
      HH.tr [HP.style "vertical-align: top;"] [ HH.td_ [ HH.text "-" ], HH.td_ [ renderProblem p.contentHash ] ]

showJust :: Maybe String -> String
showJust (Just v) = v
showJust Nothing = ""

showNewProblems :: Step -> String
showNewProblems step =
  case getStepProblems step of
    [] -> ""
    problems -> " -> " <> show (map (\p -> p.contentHash) problems)

handleAction :: forall cs o m. (MonadAff m) => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< stepsEmitter
    pure unit
  NewStep step -> H.modify_ (addStep step)

addStep :: Step -> State -> State
addStep step st = { steps: steps', problem: problem', completion: completion' }
  where
  steps' = case getStepParent step of
    Nothing -> st.steps
    Just parent -> Map.insert parent step st.steps
  problem' = case step of
    InitStep { problem } -> Just problem
    _ -> st.problem
  completion' = case step of
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
    _ <- sequence $ map (H.liftEffect <<< HS.notify listener) steps
    Aff.delay $ Aff.Milliseconds 100.0
    pure
      { howManyReadSoFar: howManyReadSoFar + (Array.length steps)
      , done: Array.any isDoneStep steps
      }

isDoneStep :: Action -> Boolean
isDoneStep (NewStep DoneStep) = true
isDoneStep _ = false

foreign import _getNewSteps :: Int -> Effect (Promise (Array String))

getNewSteps :: Int -> Aff (Array Action)
getNewSteps currLenghtRead = do
  stepTexts <- Promise.toAffE (_getNewSteps currLenghtRead)
  pure $ map NewStep $ parseSteps stepTexts
