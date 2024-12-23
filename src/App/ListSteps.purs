module App.ListSteps where

import Prelude

import Data.Maybe (Maybe(..))

import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.Subscription as HS


type Step = String -- TODO

type State = { steps :: Array Step }

data Action = Initialize | NewStep Step

component :: forall q i o m. (MonadAff m) => H.Component q i o m
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
    HH.div_ [ HH.text stepText ]

handleAction :: forall cs o m. (MonadAff m) => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    -- _ <- H.subscribe =<< getSteps
    -- pure unit
    H.modify_ \st -> st { steps = ["step 1", "step 2 is longer", "step 3"] }
  NewStep stepText -> H.modify_ \st -> st { steps = [ stepText ] <> st.steps }

getSteps :: forall m a. MonadAff m => m (HS.Emitter a)
getSteps = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    -- H.liftEffect $ HS.notify listener (NewStep ...)
    Aff.delay $ Aff.Milliseconds 1000.0
  pure emitter