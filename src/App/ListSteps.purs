module App.ListSteps where

import Prelude

import App.Utils (actOnStateUntil)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
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
    _ <- H.subscribe =<< stepsEmitter
    pure unit
  NewStep stepText -> H.modify_ \st -> st { steps = st.steps <> [ stepText ] }

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
isDoneStep (NewStep stepText) = String.contains (String.Pattern "DoneStep") stepText
isDoneStep _ = false

foreign import _getNewSteps :: Int -> Effect (Promise (Array String))

getNewSteps :: Int -> Aff (Array Action)
getNewSteps currLenghtRead = do
  stepTexts <- Promise.toAffE (_getNewSteps currLenghtRead)
  pure $ map NewStep stepTexts
