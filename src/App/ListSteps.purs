module App.ListSteps where

import App.Steps

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
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (+), (<<<), (<>), (=<<), (==))
import Web.Event.Event (Event)
import Web.Event.Event (stopPropagation)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

type State =
  { steps :: Map ProblemHash Step
  , problems :: Map ProblemHash Problem
  , initProblem :: Maybe Problem
  , completion :: Maybe String
  , focus :: Maybe ProblemHash
  }

initialState :: forall input. input -> State
initialState _ =
  { steps: Map.empty
  , problems: Map.empty
  , initProblem: Nothing
  , completion: Nothing
  , focus: Nothing
  }

data Action = Initialize | NewStep Step | Focus Event (Maybe ProblemHash)

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
  HH.div
    [ HP.style "translate: 200px 200px;" ]
    [ renderStepsAsTree state ]

renderWithPopup
  :: forall cs m
   . { popupContents :: Array (H.ComponentHTML Action cs m)
     , popupTargetElement :: H.ComponentHTML Action cs m
     }
  -> H.ComponentHTML Action cs m
renderWithPopup { popupContents, popupTargetElement } =
  HH.div [ HP.class_ (ClassName "popup") ]
    [ popupTargetElement
    , HH.span [ HP.class_ (ClassName "popupcontents show") ]
        (popupContents <> [ closeIcon ])
    ]
  where
  closeIcon = HH.div
    [ HE.onClick (\e -> Focus (toEvent e) Nothing), HP.class_ (ClassName "popupcloseicon") ]
    [ HH.text "ⓧ" ]

renderStepsAsTree :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsTree { initProblem: Nothing } = HH.text "No steps"
renderStepsAsTree st@{ initProblem: Just initProblem } =
  renderProblem initProblem.contentHash
  where
  renderProblem problemHash =
    if st.focus == Just problemHash then
      renderWithPopup
        { popupContents: [ HH.text $ problem.constraint ]
        , popupTargetElement: stepTable
        }
    else stepTable
    where
    stepTable =
      HH.table
        [ HP.style "border: 1px solid;", HE.onClick (\e -> Focus (toEvent e) (Just problemHash)) ]
        [ HH.tbody_ $
            [ HH.tr_ [ HH.td [ HP.colSpan 2 ] [ HH.text $ showStepEssence step ] ] ]
              <>
                (map renderSubProblem $ getStepProblems step)
        ]

    step = case Map.lookup problemHash st.steps of
      Just step_ -> step_
      _ -> AbortStep { detail: "No step with hash " <> problemHash }

    problem = case Map.lookup problemHash st.problems of
      Just problem_ -> problem_
      _ -> dummyProblem

    renderSubProblem p =
      HH.tr [ HP.style "vertical-align: top;" ]
        [ HH.td_ [ HH.text "-" ], HH.td_ [ renderProblem p.contentHash ] ]

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
  Focus e focus' -> do
    H.liftEffect $ Event.stopPropagation e
    H.modify_ $ \st -> st { focus = focus' }

addStep :: Step -> State -> State
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
