module View.Item
  ( view
  ) where

import Prelude

import Data.Array (delete)
import Data.Newtype (over)
import Grain (class KeyedGlobalGrain, GProxy(..), KGProxy(..), VNode, fromConstructor, useUpdater, useValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))

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

view :: Int -> VNode
view id =
  H.key (show id) $ H.fingerprint (show id) $ H.component do
    -- Store each item states globally.
    Item item <- useValue (KGProxy $ show id :: _ Item)
    updateItem <- useUpdater (KGProxy $ show id :: _ Item)
    updateItemIds <- useUpdater (GProxy :: _ ItemIds)

    let toggleClicked =
          updateItem \(Item i) ->
            Item i { clicked = not i.clicked }

        deleteItem =
          updateItemIds $ over ItemIds $ delete id

    pure $ H.li # H.kids
      [ H.button # H.onClick (const deleteItem) # H.kids [ H.text "DELETE" ]
      , H.span # H.onClick (const toggleClicked) # H.kids
          [ H.text $ item.name <> if item.clicked then " clicked!" else ""
          ]
      ]
