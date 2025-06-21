module App.StepsTree
  ( Slot,
    component,
    Query(..),
    Output(..)
  ) where

import Prelude

import App.Steps (Boxes(..), ProblemHash, MaybeStep, Step(..), getStepProblems, showStepEssence)
import App.StepsReader (StepsState)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

type State =
  { stepsState :: StepsState
  , focusedStep :: MaybeStep
  , zoomedStep :: MaybeStep
  }

type Input = StepsState

data Query a = TellNewFocusedStep MaybeStep a | TellNewZoomedStep MaybeStep a

data Output = OutputNewFocusedStepRequest MaybeStep

type Slot id = H.Slot Query Output id

initialState :: Input -> State
initialState input =
  { stepsState: input
  , focusedStep: Nothing
  , zoomedStep: Nothing
  }

data Action = NewStepsState StepsState | ClickedStep Event ProblemHash

component :: forall m. (MonadAff m) => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render: renderStepsAsTree
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< NewStepsState
        }
    }

handleAction :: forall cs m. (MonadAff m) => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  NewStepsState stepsState -> do
    H.modify_ $ \st -> st { stepsState = stepsState }
  ClickedStep e problemHash -> do
    H.liftEffect $ Event.stopPropagation e
    H.raise (OutputNewFocusedStepRequest (Just problemHash))

handleQuery :: forall a cs m. (MonadAff m) => Query a -> H.HalogenM State Action cs Output m (Maybe a)
handleQuery (TellNewFocusedStep focusedStep a) = do
  H.modify_ $ \st -> st { focusedStep = focusedStep }
  H.liftEffect $ scrollToFocusedStepIfNotVisible focusedStep
  pure (Just a)
handleQuery (TellNewZoomedStep zoomedStep a) = do
  H.modify_ $ \st -> st { zoomedStep = zoomedStep }
  pure (Just a)

scrollToFocusedStepIfNotVisible :: MaybeStep -> Effect Unit
scrollToFocusedStepIfNotVisible Nothing = pure unit
scrollToFocusedStepIfNotVisible (Just problemHash) = do
  _scrollToElementIdIfNotVisible (makeTreeProblemId problemHash)

foreign import _scrollToElementIdIfNotVisible :: String -> Effect Unit

renderStepsAsTree :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsTree { stepsState, focusedStep, zoomedStep } =
  case stepsState.initProblem of
    Nothing -> HH.text "No steps"
    Just initProblem -> renderProblemNode initProblem.contentHash
  where
  renderProblemNode problemHash =
    stepTable
    where
    hasFocus = focusedStep == Just problemHash
    isZoomedTo = zoomedStep == Just problemHash
    stepTable =
      HH.table
        [ HP.style tableStyle,
          HP.id (makeTreeProblemId problemHash),
          HE.onClick (\e -> ClickedStep (MouseEvent.toEvent e) problemHash) ]
        [ HH.tbody_ $
            [ HH.tr_ [ HH.td [ HP.colSpan 2 ] [ HH.text $ showStepEssence step ] ] ]
              <>
                (map renderSubProblem $ getStepProblems step)
        ]

    tableStyle
      | isZoomedTo && hasFocus = "border: 4px solid; border-color: red;" <> bgrStyle 
      | isZoomedTo = "border: 4px solid; border-color: blue;" <> bgrStyle 
      | hasFocus = "border: 2px solid; border-color: red;" <> bgrStyle 
      | otherwise = "border: 1px solid;" <> bgrStyle

    bgrStyle = "background-color: " <> (stepColor step) <> ";"

    step = case Map.lookup problemHash stepsState.steps of
      Just step_ -> step_
      _ -> AbortStep { detail: "No step with hash " <> problemHash }

    renderSubProblem p =
      HH.tr [ HP.style "vertical-align: top;" ]
        [ HH.td_ [ HH.text "-" ], HH.td_ [ renderProblemNode p.contentHash ] ]

stepColor :: Step -> String
stepColor = case _ of
  PruneStep { prunePaving: { inner: Boxes innerBoxes, outer: Boxes outerBoxes, undecided: [] } } ->
    if Array.null outerBoxes then "rgba(150,220,150,50)"
    else if Array.null innerBoxes then "rgba(220,150,150,50)"
    else "white"
  _ -> "white"

makeTreeProblemId :: ProblemHash -> String
makeTreeProblemId problemHash = "tree-problem-" <> problemHash