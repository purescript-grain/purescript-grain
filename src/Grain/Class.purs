module Grain.Class
  ( GProxy
  , grain
  , grainWithKey
  , grainKey
  , class Grain
  , initialState
  , typeRefOf
  ) where

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of state proxy.
data GProxy a = GProxy String

-- | A proxy to get a state, updater.
grain :: forall a. GProxy a
grain = GProxy ""

-- | A proxy to get a state, updater.
-- |
-- | The difference with `grain` is that you can manage a state per any item.
-- | The state is managed per passed key.
grainWithKey :: forall a. String -> GProxy a
grainWithKey = GProxy

-- | Get a key of `GProxy`.
-- |
-- | if it has no key, this returns empty string.
grainKey :: forall a. GProxy a -> String
grainKey (GProxy key) = key

-- | Representation of a partial state of application state.
-- |
-- | Application state is composed of this type class's instances.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class Grain a where
  initialState :: GProxy a -> Effect a
  typeRefOf :: GProxy a -> TypeRef
