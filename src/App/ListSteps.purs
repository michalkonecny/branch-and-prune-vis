module App.ListSteps where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE

type Step = String -- TODO

type State = { steps :: Array Step }

data Action = Initialize | NewStep Step

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { steps: [] }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        (map renderStep state.steps)
    ]
    where
      renderStep stepText =
        HH.div_ [HH.text stepText]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> H.modify_ \st -> st { steps = ["step 1", "step 2 is longer", "step 3"] }
  NewStep stepText -> H.modify_ \st -> st { steps = [ stepText ] <> st.steps }
