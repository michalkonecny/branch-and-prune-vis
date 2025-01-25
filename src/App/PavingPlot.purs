module App.PavingPlot
  ( Slot
  , component
  , Query(..)
  , Output(..)
  ) where

import Prelude

import App.Steps (Boxes(..), ProblemHash, Step(..), Var, dummyProblem, getStepProblems)
import App.StepsReader (StepsState)
import App.StepsTree (FocusedStep)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Web.Event.Event (Event)
import Web.Event.Event as Event
-- import Web.UIEvent.MouseEvent as MouseEvent

plotSizeX :: Number
plotSizeX = 400.0

plotSizeY :: Number
plotSizeY = 400.0

type Slot id = H.Slot Query Output id

type Input = StepsState

data Query a = TellNewFocusedStep FocusedStep a

data Output = OutputNewFocusedStepRequest FocusedStep

type State =
  { stepsState :: StepsState
  , focusedStep :: FocusedStep
  , plotX :: Var
  , plotY :: Var
  }

initialState :: Input -> State
initialState input =
  { stepsState: input
  , focusedStep: Nothing
  , plotX: "x"
  , plotY: "y"
  }

component :: forall m. (MonadAff m) => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render: renderStepsAsBoxes
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< NewStepsState
        }
    }

data Action = NewStepsState StepsState | ClickedStep Event ProblemHash

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
  pure (Just a)

renderStepsAsBoxes :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsBoxes { stepsState, focusedStep, plotX, plotY } =
  case stepsState.initProblem of
    Nothing -> HH.text "No steps"
    Just initProblem -> renderBoxes initProblem
  where
  renderBoxes initProblem =
    HH.div_
      [ SE.svg
          [ SA.width (plotSizeX + 5.0)
          , SA.height (plotSizeY + 5.0)
          , SA.viewBox (-2.0) (-2.0) (plotSizeX + 3.0) (plotSizeY + 3.0)
          ]
          (pavingElements <> overlayElements)
      ]
    where
    initVarDomains = initProblem.scope.varDomains
    initXrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup plotX initVarDomains)
    initYrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup plotY initVarDomains)
    initXspan = initXrange.u - initXrange.l
    initYspan = initYrange.u - initYrange.l

    toScreenX x = (x - initXrange.l) * plotSizeX / initXspan
    toScreenY y = (y - initYrange.l) * plotSizeY / initYspan
    toScreenRangeX { l, u } = { l: toScreenX l, u: toScreenX u }
    toScreenRangeY { l, u } = { l: toScreenY l, u: toScreenY u }

    rectsForProblemHash problemHash =
      { pavingElements: [ mainRect ] <> subBoxPaving
      , overlayElements: subBoxOverlay <> (if hasFocus then [ outlineRect ] else [])
      }
      where
      subBoxPaving = Array.concat $ map (\x -> x.pavingElements) subBoxPavingsAndOverlays
      subBoxOverlay = Array.concat $ map (\x -> x.overlayElements) subBoxPavingsAndOverlays
      subBoxPavingsAndOverlays = map rectsForSubproblem (getStepProblems step)
      hasFocus = focusedStep == Just problemHash
      boxRectAttributes =
        [ SA.x xScreenRange.l
        , SA.y yScreenRange.l
        , SA.width (xScreenRange.u - xScreenRange.l)
        , SA.height (yScreenRange.u - yScreenRange.l)
        ]

      mainRect = SE.rect $
        boxRectAttributes <>
          [ SA.stroke $ Named "black"
          , SA.strokeWidth 1.0
          , SA.fill color
          , SA.fillOpacity 0.2
          ]

      outlineRect = SE.rect $
        boxRectAttributes <>
          [ SA.stroke $ Named "red"
          , SA.strokeWidth 2.0
          , SA.fill color
          , SA.fillOpacity 0.0
          ]

      step = case Map.lookup problemHash stepsState.steps of
        Just step_ -> step_
        _ -> AbortStep { detail: "No step with hash " <> problemHash }

      color = case step of
        PruneStep { prunePaving: { inner: Boxes innerBoxes, outer: Boxes outerBoxes, undecided: [] } } ->
          if Array.null outerBoxes then Named "green"
          else if Array.null innerBoxes then Named "red"
          else Named "white"
        _ -> Named "white"

      problem = case Map.lookup problemHash stepsState.problems of
        Just problem_ -> problem_
        _ -> dummyProblem

      rectsForSubproblem subProblem = rectsForProblemHash subProblem.contentHash

      varDomains = problem.scope.varDomains

      xrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup plotX varDomains)
      yrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup plotY varDomains)
      xScreenRange = toScreenRangeX xrange
      yScreenRange = toScreenRangeY yrange

    { pavingElements, overlayElements } = rectsForProblemHash initProblem.contentHash
