module State.Item where

import Prelude

import Grain (class GlobalGrain, class KeyedGlobalGrain, fromConstructor)

newtype Item = Item
  { name :: String
  , clicked :: Boolean
  }

instance globalGrainItem :: GlobalGrain Item where
  initialState _ =
    pure $ Item
      { name: "Globally stored Item"
      , clicked: false
      }
  typeRefOf _ = fromConstructor Item

instance keyedGlobalGrainItem :: KeyedGlobalGrain String Item
