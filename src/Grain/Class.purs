module Grain.Class
  ( GProxy(..)
  , grain
  , grainWithKey
  , class Grain
  , initialState
  , typeRefOf
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of state proxy.
data GProxy a = GProxy (Maybe String)

-- | A proxy to get a state, updater.
grain :: forall a. GProxy a
grain = GProxy Nothing

-- | A proxy to get a state, updater.
-- |
-- | The difference with `grain` is that you can manage a state per any item.
-- | The state is managed per passed key.
grainWithKey :: forall a. String -> GProxy a
grainWithKey = GProxy <<< Just

-- | Representation of a partial state of application state.
-- |
-- | Application state is composed of this type class's instances.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class Grain a where
  initialState :: GProxy a -> Effect a
  typeRefOf :: GProxy a -> TypeRef
