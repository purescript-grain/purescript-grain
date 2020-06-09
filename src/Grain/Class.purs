module Grain.Class
  ( class Grain
  , initialState
  , typeRefOf
  ) where

import Prelude

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | The representation of type as state key and type of state to it.
-- | This type class uses `Show` and `TypeRef` for state key.
class Show k <= Grain k a | k -> a where
  initialState :: k -> Effect a
  typeRefOf :: k -> TypeRef
