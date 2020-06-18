module State.Item where

import Prelude

import Grain (class KeyedGlobalGrain, KGProxy(..), fromConstructor)

newtype Item = Item
  { name :: String
  , clicked :: Boolean
  }

instance keyGlobalGrainItem :: KeyedGlobalGrain Item where
  initialState (KGProxy key) =
    pure $ Item
      { name: "Globally stored Item" <> key
      , clicked: false
      }
  typeRefOf _ = fromConstructor Item
