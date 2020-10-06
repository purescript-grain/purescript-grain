module Grain.Class.GlobalGrain
  ( GProxy(..)
  , class GlobalGrain
  , initialState
  , typeRefOf
  ) where

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of global state proxy.
data GProxy a = GProxy

-- | Representation of a partial state of application state.
-- |
-- | You can use this to define global state.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class GlobalGrain a where
  initialState :: GProxy a -> Effect a
  typeRefOf :: GProxy a -> TypeRef
