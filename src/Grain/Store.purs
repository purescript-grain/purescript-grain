module Grain.Store
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
import Foreign.Object (Object, empty, insert, lookup)
import Grain.Class (class Grain, initialState, typeRefOf)
import Grain.Emitter (Emitter, createEmitter, emit, subscribe, unsubscribe)
import Grain.TypeKeyRef (TypeKeyRef)
import Grain.TypeKeyRef as TKRef
import Unsafe.Coerce (unsafeCoerce)

newtype Store = Store
  { typeKeyRef :: TypeKeyRef
  , partsRef :: Ref (Object Part)
  }

type Part =
  { emitter :: Emitter
  , valueRef :: Ref Foreign
  }

createStore :: Effect Store
createStore = do
  typeKeyRef <- TKRef.new
  partsRef <- Ref.new empty
  pure $ Store { typeKeyRef, partsRef }

readGrain
  :: forall k a
   . Grain k a
  => k
  -> Store
  -> Effect a
readGrain k store = do
  { valueRef } <- lookupPart k store
  value <- Ref.read valueRef
  pure $ unsafeFromForeign value

subscribeGrain
  :: forall k a
   . Grain k a
  => k
  -> Effect Unit
  -> Store
  -> Effect Unit
subscribeGrain k listener store = do
  { emitter } <- lookupPart k store
  subscribe listener emitter

unsubscribeGrain
  :: forall k a
   . Grain k a
  => k
  -> Effect Unit
  -> Store
  -> Effect Unit
unsubscribeGrain k listener store = do
  { emitter } <- lookupPart k store
  unsubscribe listener emitter

updateGrain
  :: forall k a
   . Grain k a
  => k
  -> (a -> a)
  -> Store
  -> Effect Unit
updateGrain k f store = do
  { emitter, valueRef } <- lookupPart k store
  Ref.modify_ (unsafeCoerce f) valueRef
  emit emitter

readPartKey
  :: forall k a
   . Grain k a
  => k
  -> Store
  -> Effect String
readPartKey k (Store { typeKeyRef }) = do
  typeKey <- TKRef.read (typeRefOf k) typeKeyRef
  pure $ show k <> "-" <> typeKey

lookupPart
  :: forall k a
   . Grain k a
  => k
  -> Store
  -> Effect Part
lookupPart k store@(Store { partsRef }) = do
  partKey <- readPartKey k store
  maybePart <- lookup partKey <$> Ref.read partsRef
  case maybePart of
    Just part -> pure part
    Nothing -> do
      value <- initialState k
      valueRef <- Ref.new $ unsafeToForeign value
      emitter <- createEmitter
      let part = { emitter, valueRef }
      Ref.modify_ (insert partKey part) partsRef
      pure part
