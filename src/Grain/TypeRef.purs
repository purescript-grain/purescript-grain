module Grain.TypeRef
  ( TypeRef
  , fromConstructor
  ) where

import Unsafe.Coerce (unsafeCoerce)

-- | Reference of any type.
foreign import data TypeRef :: Type

-- | Create a `TypeRef`.
-- |
-- | Treat a constructor function as reference of type.
fromConstructor :: forall a. a -> TypeRef
fromConstructor = unsafeCoerce
