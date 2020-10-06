module Grain.Class where

import Effect (Effect)
import Grain.Class.GlobalGrain (class GlobalGrain, GProxy)
import Grain.Class.GlobalGrain as G
import Grain.Class.LocalGrain (class LocalGrain, LProxy)
import Grain.Class.LocalGrain as L
import Grain.TypeRef (TypeRef)

class Grain p a where
  initialState :: p a -> Effect a
  typeRefOf :: p a -> TypeRef
  which :: forall b. p a -> { global :: b, local :: b } -> b

instance grainGProxy :: GlobalGrain a => Grain GProxy a where
  initialState = G.initialState
  typeRefOf = G.typeRefOf
  which _ { global } = global

instance grainLProxy :: LocalGrain a => Grain LProxy a where
  initialState = L.initialState
  typeRefOf = L.typeRefOf
  which _ { local } = local
