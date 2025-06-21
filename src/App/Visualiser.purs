module App.Visualiser (component) where

import Prelude

import App.PavingPlot as PavingPlot
import App.StepDetail as StepDetail
import App.Steps (MaybeStep)
import App.StepsReader (StepsState)
import App.StepsReader as StepsReader
import App.StepsTree as StepsTree
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as BS5
import Type.Proxy (Proxy(..))

type Slots =
  ( stepsTree :: StepsTree.Slot Int
  , stepsReader :: StepsReader.Slot Int
  , pavingPlot :: PavingPlot.Slot Int
  , stepDetail :: StepDetail.Slot Int
  )

_stepsTree = Proxy :: Proxy "stepsTree"
_stepsReader = Proxy :: Proxy "stepsReader"
_pavingPlot = Proxy :: Proxy "pavingPlot"
_stepDetail = Proxy :: Proxy "stepDetail"

_stepsReaderId ∷ Int
_stepsReaderId = 0

_stepsTreeId ∷ Int
_stepsTreeId = 1

_pavingPlotId :: Int
_pavingPlotId = 2

_stepDetailId :: Int
_stepDetailId = 3

type State =
  { stepsState :: StepsState
  , focusedStep :: MaybeStep
  , zoomedStep :: MaybeStep
  }

initialState :: forall i. i -> State
initialState _ =
  { stepsState: StepsReader.initialState unit
  , focusedStep: Nothing
  , zoomedStep: Nothing
  }

getStepInfo :: State -> StepDetail.Input
getStepInfo { stepsState, focusedStep } =
  case focusedStep of
    Nothing -> Nothing
    Just problemHash ->
      case Map.lookup problemHash stepsState.problems of
        Nothing -> Nothing
        Just problem ->
          case Map.lookup problemHash stepsState.steps of
            Nothing -> Nothing
            Just step ->
              Just { problem, step }

data Action = NewStepsState StepsState | NewFocusedStep MaybeStep | NewZoomedStep MaybeStep

component :: forall q i o m. (MonadAff m) => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render: renderVisualiser
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

handleAction :: forall m o. (MonadAff m) => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  NewStepsState stepsState -> do
    H.modify_ $ \st -> st { stepsState = stepsState }
  NewFocusedStep focusedStep -> do
    H.modify_ $ \st -> st { focusedStep = focusedStep }
    H.tell _stepsTree _stepsTreeId (StepsTree.TellNewFocusedStep focusedStep)
    H.tell _pavingPlot _pavingPlotId (PavingPlot.TellNewFocusedStep focusedStep)
  NewZoomedStep zoomedStep -> do
    liftEffect $ log "zoomedStep"
    H.modify_ $ \st -> st { zoomedStep = zoomedStep }
    H.tell _stepsTree _stepsTreeId (StepsTree.TellNewZoomedStep zoomedStep)
    H.tell _pavingPlot _pavingPlotId (PavingPlot.TellNewZoomedStep zoomedStep)

handleStepsReaderOutput :: StepsReader.Output -> Action
handleStepsReaderOutput = case _ of
  (StepsReader.OutputNewStepsState stepsState) -> NewStepsState stepsState

handleStepsTreeOutput :: StepsTree.Output -> Action
handleStepsTreeOutput = case _ of
  (StepsTree.OutputNewFocusedStepRequest focusedStep) -> NewFocusedStep focusedStep

handlePavingPlotOutput :: PavingPlot.Output -> Action
handlePavingPlotOutput = case _ of
  (PavingPlot.OutputNewFocusedStepRequest focusedStep) -> NewFocusedStep focusedStep
  (PavingPlot.OutputNewZoomedStepRequest zoomedStep) -> NewZoomedStep zoomedStep

renderVisualiser :: forall m. (MonadAff m) => State -> H.ComponentHTML Action Slots m
renderVisualiser state =
  HH.div [ HP.classes [ BS5.m1 ] ]
    [ HH.div [ HP.classes [ BS5.row ] ]
        [ HH.div [ HP.classes [ BS5.col3, BS5.overflowAuto ], HP.style "height: 650px" ]
            [ HH.slot _stepsReader _stepsReaderId StepsReader.component unit handleStepsReaderOutput
            , HH.slot _stepsTree _stepsTreeId StepsTree.component state.stepsState handleStepsTreeOutput
            ]
        , HH.div [ HP.classes [ BS5.col4, BS5.overflowAuto ], HP.style "height: 650px" ]
            [ HH.slot_ _stepDetail _stepDetailId StepDetail.component (getStepInfo state) ]
        , HH.div [ HP.classes [ BS5.col5, BS5.overflowAuto ] ]
            [ HH.slot _pavingPlot _pavingPlotId PavingPlot.component state.stepsState handlePavingPlotOutput ]
        ]
    ]
