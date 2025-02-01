module App.StepDetail
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , State
  , StepInfo(..)
  , component
  ) where

import Prelude

import App.Form (Form(..))
import App.Steps (Interval, Problem, Step, Var)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Maybe StepInfo

type StepInfo =
  { step :: Step
  , problem :: Problem
  }

type Input = State

data Query a = VoidQuery a

type Output = Void

type Slot id = H.Slot Query Output id

initialState :: Input -> State
initialState _ = Nothing

data Action = NewState State

component :: forall m. (MonadAff m) => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render: renderFocusedStep
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< NewState
        }
    }

handleAction :: forall cs m. (MonadAff m) => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  NewState state -> do
    H.put state

renderFocusedStep :: forall cs m. State -> H.ComponentHTML Action cs m
renderFocusedStep Nothing = HH.div_ []
renderFocusedStep (Just { problem }) =
  HH.div_
    (boxDescription <> [ HH.div_ $ renderForm problem.constraint ])
  where
  (varRanges :: Array _) = Map.toUnfoldable $ problem.scope.varDomains
  boxDescription = Array.concat $ map describeVar varRanges

  describeVar :: (Tuple Var Interval) -> Array _
  describeVar (Tuple var { l, u }) = [ HH.text descr, HH.br_ ]
    where
    descr = var <> " ∈ [ " <> (show l) <> ", " <> show u <> " ]"

renderForm :: forall cs m. Form -> Array (H.ComponentHTML Action cs m)
renderForm FormTrue = [ HH.text "True" ]
renderForm FormFalse = [ HH.text "False" ]
renderForm (FormComp { comp, e1, e2 }) =
  [ HH.text $ e1 <> " " <> show comp <> " " <> e2 ]
renderForm (FormUnary { uconn, f1 }) =
  [ HH.text $ show uconn, HH.div [ formStyle ] f1H ]
  where
  f1H = renderForm f1
renderForm (FormBinary { bconn, f1, f2 }) =
  [ HH.div [ formStyle ] f1H
  , HH.text $ show bconn
  , HH.div [ formStyle ] f2H
  ]
  where
  f1H = renderForm f1
  f2H = renderForm f2
renderForm (FormIfThenElse { fc, ft, ff }) =
  [ HH.div [ formStyle ] $ [ HH.text "if " ] <> fcH
  , HH.div [ formStyle ] $ [ HH.text "then " ] <> ftH
  , HH.div [ formStyle ] $ [ HH.text "else " ] <> ffH
  ]
  where
  fcH = renderForm fc
  ftH = renderForm ft
  ffH = renderForm ff

formStyle :: _
formStyle = HP.style "border-style: dotted; margin: 5px;"