module App.PavingPlot
  ( Slot
  , component
  , Query(..)
  , Output(..)
  ) where

import Prelude

import App.Steps (Boxes(..), MaybeStep, Step(..), Var, ProblemHash, dummyProblem, getStepProblems)
import App.StepsReader (StepsState)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Themes.Bootstrap5 as BS5

plotSizeX :: Number
plotSizeX = 400.0

plotSizeY :: Number
plotSizeY = 350.0

type Slot id = H.Slot Query Output id

type Input = StepsState

data Query a = TellNewFocusedStep MaybeStep a | TellNewZoomedStep MaybeStep a

data Output = OutputNewFocusedStepRequest MaybeStep | OutputNewZoomedStepRequest MaybeStep

type State =
  { stepsState :: StepsState
  , focusedStep :: MaybeStep
  , zoomedStep :: MaybeStep
  , plotX :: Maybe Var
  , plotY :: Maybe Var
  }

initialState :: Input -> State
initialState input =
  { stepsState: input
  , focusedStep: Nothing
  , zoomedStep: Nothing
  , plotX: Nothing
  , plotY: Nothing
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

data Action = NewStepsState StepsState | ClickedStep (Maybe ProblemHash) | ZoomTo (Maybe ProblemHash)

handleAction :: forall cs m. (MonadAff m) => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  NewStepsState stepsState -> do
    H.modify_ $ updateStepsState stepsState
  ClickedStep maybeProblemHash -> do
    H.raise (OutputNewFocusedStepRequest maybeProblemHash)
  ZoomTo maybeProblemHash -> do
    H.raise (OutputNewZoomedStepRequest maybeProblemHash)

updateStepsState :: StepsState -> State -> State
updateStepsState stepsState st
  | st.plotX == Nothing && st.plotY == Nothing =
      -- plot variables not set yet
      -- set them to the first variables in the initial problem
      let
        vars = case stepsState.initProblem of
          Nothing -> []
          Just problem -> problem.scope.splitOrder
      in
        st
          { stepsState = stepsState
          , plotX = vars Array.!! 0
          , plotY = vars Array.!! 1
          }
  | otherwise = st { stepsState = stepsState }

handleQuery :: forall a cs m. (MonadAff m) => Query a -> H.HalogenM State Action cs Output m (Maybe a)
handleQuery (TellNewFocusedStep focusedStep a) = do
  H.modify_ $ \st -> st { focusedStep = focusedStep }
  pure (Just a)
handleQuery (TellNewZoomedStep zoomedStep a) = do
  H.modify_ $ \st -> st { zoomedStep = zoomedStep }
  pure (Just a)

renderStepsAsBoxes :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsBoxes { stepsState, focusedStep, zoomedStep, plotX, plotY } =
  case stepsState.initProblem of
    Nothing -> HH.text "No steps"
    Just initProblem ->
      case zoomedStep of
        Just zoomedProblemHash ->
          case Map.lookup zoomedProblemHash stepsState.problems of
            Just zoomedProblem -> renderBoxes zoomedProblem
            _ -> renderBoxes initProblem
        _ -> renderBoxes initProblem
  where
  renderBoxes initProblem =
    HH.div [ HP.classes [ BS5.dFlex ] ]
      [ HH.table_
          [ HH.tbody_
              [ HH.tr_ [ HH.td_ [ yAxisLabel ], HH.td_ [ plot ] ]
              , HH.tr_
                  [ HH.td_ []
                  , HH.td [ HP.style "text-align: center;" ] [ xAxisLabel ]
                  ]
              ]
          ]
      , HH.div [ HP.classes [ BS5.dFlex, BS5.flexColumn ] ]
          [ HH.button [ HE.onClick (\_ -> ZoomTo focusedStep) ] [ HH.text "🔍" ]
          , HH.button [ HE.onClick (\_ -> ZoomTo Nothing) ] [ HH.text "X" ]
          ]
      ]
    where
    xAxisLabel = HH.div
      [ HP.classes [ BS5.m1 ] ]
      [ HH.text $ maybe "" identity plotX ]
    yAxisLabel = HH.div
      [ HP.classes [ BS5.m1 ], HP.style "transform: rotate(90deg);" ]
      [ HH.text $ maybe "" identity plotY ]
    plot = SE.svg
      [ SA.width (plotSizeX + 5.0)
      , SA.height (plotSizeY + 5.0)
      , SA.viewBox (-2.0) (-2.0) (plotSizeX + 3.0) (plotSizeY + 3.0)
      ]
      (pavingElements <> overlayElements)
      where
      initVarDomains = initProblem.scope.varDomains
      initXrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (plotX >>= flip Map.lookup initVarDomains)
      initYrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (plotY >>= flip Map.lookup initVarDomains)
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
            , HE.onClick (\_ -> ClickedStep (Just problemHash)) -- select this leaf
            ]

        outlineRect = SE.rect $
          boxRectAttributes <>
            [ SA.stroke $ Named "red"
            , SA.strokeWidth 2.0
            , SA.fill color
            , SA.fillOpacity 0.0
            , HE.onClick (\_ -> ClickedStep Nothing) -- unselect
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

        xrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (plotX >>= flip Map.lookup varDomains)
        yrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (plotY >>= flip Map.lookup varDomains)
        xScreenRange = toScreenRangeX xrange
        yScreenRange = toScreenRangeY yrange

      { pavingElements, overlayElements } = rectsForProblemHash initProblem.contentHash
