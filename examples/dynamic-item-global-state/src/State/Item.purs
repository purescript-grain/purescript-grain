module State.Item where

import Prelude

import Grain (class KeyedGlobalGrain, fromConstructor)

newtype Item = Item
  { name :: String
  , clicked :: Boolean
  }

instance keyedGlobalGrainItem :: KeyedGlobalGrain String Item where
  initialState _ =
    pure $ Item
      { name: "Globally stored Item"
      , clicked: false
      }
  typeRefOf _ = fromConstructor Item
