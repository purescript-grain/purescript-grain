module Grain.Class where

import Effect (Effect)
import Grain.Class.GProxy (class GlobalGrain, GProxy)
import Grain.Class.GProxy as G
import Grain.Class.KGProxy (class KeyedGlobalGrain, KGProxy(..))
import Grain.Class.KGProxy as KG
import Grain.Class.LProxy (class LocalGrain, LProxy)
import Grain.Class.LProxy as L
import Grain.TypeRef (TypeRef)

class Grain p a where
  initialState :: p a -> Effect a
  typeRefOf :: p a -> TypeRef
  keyOf :: p a -> String
  which :: forall b. p a -> { global :: b, local :: b } -> b

instance grainGProxy :: GlobalGrain a => Grain GProxy a where
  initialState = G.initialState
  typeRefOf = G.typeRefOf
  keyOf _ = "__DEFAULT__"
  which _ { global } = global

instance grainKGProxy :: KeyedGlobalGrain a => Grain KGProxy a where
  initialState = KG.initialState
  typeRefOf = KG.typeRefOf
  keyOf (KGProxy key) = key
  which _ { global } = global

instance grainLProxy :: LocalGrain a => Grain LProxy a where
  initialState = L.initialState
  typeRefOf = L.typeRefOf
  keyOf _ = "__DEFAULT__"
  which _ { local } = local
