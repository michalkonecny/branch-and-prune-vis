module App.StepDetail
  ( Slot
  , component
  , Input
  , State
  , StepInfo(..)
  , Query(..)
  , Output(..)
  ) where

import Prelude

import App.Steps (Interval, Problem, Step, Var)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH


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
    (boxDescription <> [ HH.text $ problem.constraint ])
  where
  (varRanges :: Array _) = Map.toUnfoldable $ problem.scope.varDomains
  boxDescription = Array.concat $ map describeVar varRanges

  describeVar :: (Tuple Var Interval) -> Array _
  describeVar (Tuple var { l, u }) = [ HH.text descr, HH.br_ ]
    where
    descr = var <> " ∈ [ " <> (show l) <> ", " <> show u <> " ]"
