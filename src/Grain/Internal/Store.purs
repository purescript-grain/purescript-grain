module Grain.Internal.Store
  ( Store
  , createStore
  , readGrain
  , subscribeGrain
  , unsubscribeGrain
  , updateGrain
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Grain.Class (class Grain, initialState, keyOf, typeRefOf)
import Grain.Internal.Emitter (Emitter, createEmitter, emit, subscribe, unsubscribe)
import Grain.Internal.MMap (MMap)
import Grain.Internal.MMap as MM
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.TypeRef (TypeRef)
import Unsafe.Coerce (unsafeCoerce)

newtype Store = Store (MMap TypeRef (MObject Part))

type Part =
  { emitter :: Emitter
  , valueRef :: Ref Foreign
  }

createStore :: Effect Store
createStore = Store <$> MM.new

readGrain
  :: forall p a
   . Grain p a
  => p a
  -> Store
  -> Effect a
readGrain proxy store = do
  { valueRef } <- lookupPart proxy store
  value <- Ref.read valueRef
  pure $ unsafeFromForeign value

subscribeGrain
  :: forall p a
   . Grain p a
  => p a
  -> Effect Unit
  -> Store
  -> Effect Unit
subscribeGrain proxy listener store = do
  { emitter } <- lookupPart proxy store
  subscribe listener emitter

unsubscribeGrain
  :: forall p a
   . Grain p a
  => p a
  -> Effect Unit
  -> Store
  -> Effect Unit
unsubscribeGrain proxy listener store = do
  { emitter } <- lookupPart proxy store
  unsubscribe listener emitter

updateGrain
  :: forall p a
   . Grain p a
  => p a
  -> (a -> a)
  -> Store
  -> Effect Unit
updateGrain proxy f store = do
  { emitter, valueRef } <- lookupPart proxy store
  Ref.modify_ (unsafeCoerce f) valueRef
  emit emitter

lookupPart
  :: forall p a
   . Grain p a
  => p a
  -> Store
  -> Effect Part
lookupPart proxy store = do
  partsRef <- lookupPartsRef proxy store
  mPart <- MO.get (keyOf proxy) partsRef
  case mPart of
    Just part -> pure part
    Nothing -> do
      value <- initialState proxy
      valueRef <- Ref.new $ unsafeToForeign value
      emitter <- createEmitter
      let part = { emitter, valueRef }
      MO.set (keyOf proxy) part partsRef
      pure part

lookupPartsRef
  :: forall p a
   . Grain p a
  => p a
  -> Store
  -> Effect (MObject Part)
lookupPartsRef proxy (Store m) = do
  maybePartsRef <- MM.get (typeRefOf proxy) m
  case maybePartsRef of
    Just partsRef ->
      pure partsRef
    Nothing -> do
      partsRef <- MO.new
      MM.set (typeRefOf proxy) partsRef m
      pure partsRef
