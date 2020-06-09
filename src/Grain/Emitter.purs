module Grain.Emitter
  ( Emitter
  , createEmitter
  , subscribe
  , unsubscribe
  , emit
  ) where

import Prelude

import Data.Array (filter, snoc)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Ref (Ref, modify_, new, read)
import Unsafe.Reference (unsafeRefEq)

newtype Emitter = Emitter (Ref (Array (Effect Unit)))

createEmitter :: Effect Emitter
createEmitter = Emitter <$> new []

subscribe :: Effect Unit -> Emitter -> Effect Unit
subscribe listener (Emitter listenersRef) =
  modify_ (flip snoc listener) listenersRef

unsubscribe :: Effect Unit -> Emitter -> Effect Unit
unsubscribe listener (Emitter listenersRef) =
  modify_ (filter (not <<< unsafeRefEq listener)) listenersRef

emit :: Emitter -> Effect Unit
emit (Emitter listenersRef) = do
  listeners <- read listenersRef
  sequence_ listeners
