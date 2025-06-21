module App.Utils where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))

actOnStateForever :: forall m s b. MonadRec m => { initState :: s, action :: (s -> m s) } -> m b
actOnStateForever { initState, action } =
  tailRecM (\s -> action s >>= (pure <<< Loop)) initState

actOnStateUntil
  :: forall m s
   . MonadRec m
  => { shouldFinish :: (s -> Boolean)
     , initState :: s
     , action :: (s -> m s)
     }
  -> m Unit
actOnStateUntil { shouldFinish, initState, action } =
  tailRecM doAction initState
  where
  doAction s = do
    newS <- action s
    if shouldFinish newS then
      pure $ Done unit
    else
      pure $ Loop newS
