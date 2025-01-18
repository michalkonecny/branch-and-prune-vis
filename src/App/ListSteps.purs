module App.ListSteps
  ( Action(..)
  , State
  , _getNewSteps
  , addStep
  , component
  , getNewSteps
  , handleAction
  , initialState
  , isDoneStep
  , plotSizeX
  , render
  , renderStepsAsBoxes
  , renderStepsAsTree
  , showJust
  , showNewProblems
  , stepsEmitter
  ) where

import Prelude

import App.Steps (Boxes(..), Interval, Problem, ProblemHash, Step(..), Var, dummyProblem, getStepParent, getStepProblems, parseSteps, showStepEssence)
import App.UIUtils (renderWithPopup)
import App.Utils (actOnStateUntil)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

type State =
  { steps :: Map ProblemHash Step
  , problems :: Map ProblemHash Problem
  , initProblem :: Maybe Problem
  , completion :: Maybe String
  , focus :: Maybe ProblemHash
  , plotX :: Var
  , plotY :: Var
  }

initialState :: forall input. input -> State
initialState _ =
  { steps: Map.empty
  , problems: Map.empty
  , initProblem: Nothing
  , completion: Nothing
  , focus: Nothing
  , plotX: "x"
  , plotY: "y"
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
  HH.table
    [ HP.style "translate: 50px 20px;" ]
    [ HH.tbody_
        [ HH.tr_
            [ HH.td
                [ HP.rowSpan 2 ] -- the tree spans 2 rows, next to the plot and the problem details
                [ HH.div
                    [ HP.style "overflow:scroll; width: 400px;height:600px;" ]
                    [ renderStepsAsTree state ]
                ]
            , HH.td_
                [ HH.div
                    [ HP.style "margin-left: 20px;" ]
                    [ renderStepsAsBoxes state ]
                ]
            ]
        , HH.tr_
            [ HH.td_
                [ HH.div
                    [ HP.style "margin-left: 20px; overflow:scroll; width:450px;height:170px;" ]
                    (renderFocusedProblem state)
                ]
            ]
        ]
    ]

plotSizeX :: Number
plotSizeX = 400.0

plotSizeY :: Number
plotSizeY = 400.0

renderStepsAsBoxes :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsBoxes { initProblem: Nothing } = HH.text "No steps"
renderStepsAsBoxes st@{ initProblem: Just initProblem } =
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
  initXrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup st.plotX initVarDomains)
  initYrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup st.plotY initVarDomains)
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
    hasFocus = st.focus == Just problemHash
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

    step = case Map.lookup problemHash st.steps of
      Just step_ -> step_
      _ -> AbortStep { detail: "No step with hash " <> problemHash }

    color = case step of
      PruneStep { prunePaving: { inner: Boxes innerBoxes, outer: Boxes outerBoxes, undecided: [] } } ->
        if Array.null outerBoxes then Named "green"
        else if Array.null innerBoxes then Named "red"
        else Named "white"
      _ -> Named "white"

    problem = case Map.lookup problemHash st.problems of
      Just problem_ -> problem_
      _ -> dummyProblem

    rectsForSubproblem subProblem = rectsForProblemHash subProblem.contentHash

    varDomains = problem.scope.varDomains

    xrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup st.plotX varDomains)
    yrange = maybe { l: 0.0, u: 1.0 } (\x -> x) (Map.lookup st.plotY varDomains)
    xScreenRange = toScreenRangeX xrange
    yScreenRange = toScreenRangeY yrange

  { pavingElements, overlayElements } = rectsForProblemHash initProblem.contentHash

renderFocusedProblem :: forall cs m. State -> Array (H.ComponentHTML Action cs m)
renderFocusedProblem st@{ focus } =
  case focus of 
    Just problemHash -> problemDescription problemHash
    _ -> []
  where
  problemDescription problemHash =
    boxDescription <> [ HH.text $ problem.constraint ]
    where
    problem = case Map.lookup problemHash st.problems of
      Just problem_ -> problem_
      _ -> dummyProblem

    (varRanges :: Array _) = Map.toUnfoldable $ problem.scope.varDomains
    boxDescription = Array.concat $ map describeVar varRanges

    describeVar :: (Tuple Var Interval) -> Array _
    describeVar (Tuple var { l, u }) = [ HH.text descr, HH.br_ ]
      where
      descr = var <> " ∈ [ " <> (show l) <> ", " <> show u <> " ]"

renderStepsAsTree :: forall cs m. State -> H.ComponentHTML Action cs m
renderStepsAsTree { initProblem: Nothing } = HH.text "No steps"
renderStepsAsTree st@{ initProblem: Just initProblem } =
  renderProblemNode initProblem.contentHash
  where
  renderProblemNode problemHash =
    stepTable
    where
    hasFocus = st.focus == Just problemHash
    stepTable =
      HH.table
        [ HP.style tableStyle, HE.onClick (\e -> Focus (toEvent e) (Just problemHash)) ]
        [ HH.tbody_ $
            [ HH.tr_ [ HH.td [ HP.colSpan 2 ] [ HH.text $ showStepEssence step ] ] ]
              <>
                (map renderSubProblem $ getStepProblems step)
        ]

    tableStyle
      | hasFocus = "border: 2px solid;"
      | otherwise = "border: 1px solid;"

    step = case Map.lookup problemHash st.steps of
      Just step_ -> step_
      _ -> AbortStep { detail: "No step with hash " <> problemHash }

    renderSubProblem p =
      HH.tr [ HP.style "vertical-align: top;" ]
        [ HH.td_ [ HH.text "-" ], HH.td_ [ renderProblemNode p.contentHash ] ]

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
