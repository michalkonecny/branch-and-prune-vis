module App.PlotTest where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- import Halogen.HTML.Events as HE

type Step = String -- TODO

type State = { steps :: Array Step }

data Action = Initialize

foreign import _getStepPlotDivId :: Array Int -> String

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
  HH.div [ HP.id (_getStepPlotDivId (map String.length state.steps)), HP.style "width:800px;height:600px;" ] []

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ \st -> st { steps = ["step 1", "step 2 longer", "step 3", "step 4 up"] }

