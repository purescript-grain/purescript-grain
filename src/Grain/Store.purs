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
import Grain.Class (class Grain, GProxy(..), initialState, typeRefOf)
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
  :: forall a
   . Grain a
  => GProxy a
  -> Store
  -> Effect a
readGrain proxy store = do
  { valueRef } <- lookupPart proxy store
  value <- Ref.read valueRef
  pure $ unsafeFromForeign value

subscribeGrain
  :: forall a
   . Grain a
  => GProxy a
  -> Effect Unit
  -> Store
  -> Effect Unit
subscribeGrain proxy listener store = do
  { emitter } <- lookupPart proxy store
  subscribe listener emitter

unsubscribeGrain
  :: forall a
   . Grain a
  => GProxy a
  -> Effect Unit
  -> Store
  -> Effect Unit
unsubscribeGrain proxy listener store = do
  { emitter } <- lookupPart proxy store
  unsubscribe listener emitter

updateGrain
  :: forall a
   . Grain a
  => GProxy a
  -> (a -> a)
  -> Store
  -> Effect Unit
updateGrain proxy f store = do
  { emitter, valueRef } <- lookupPart proxy store
  Ref.modify_ (unsafeCoerce f) valueRef
  emit emitter

readPartKey
  :: forall a
   . Grain a
  => GProxy a
  -> Store
  -> Effect String
readPartKey proxy (Store { typeKeyRef }) = do
  typeKey <- TKRef.read (typeRefOf proxy) typeKeyRef
  pure case proxy of
    GProxy Nothing -> typeKey
    GProxy (Just key) -> typeKey <> "-" <> key

lookupPart
  :: forall a
   . Grain a
  => GProxy a
  -> Store
  -> Effect Part
lookupPart proxy store@(Store { partsRef }) = do
  partKey <- readPartKey proxy store
  maybePart <- lookup partKey <$> Ref.read partsRef
  case maybePart of
    Just part -> pure part
    Nothing -> do
      value <- initialState proxy
      valueRef <- Ref.new $ unsafeToForeign value
      emitter <- createEmitter
      let part = { emitter, valueRef }
      Ref.modify_ (insert partKey part) partsRef
      pure part
