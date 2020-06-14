module View.Item
  ( view
  ) where

import Prelude

import Data.Array (delete)
import Data.Newtype (over)
import Grain (class Grain, VNode, fromConstructor, grain, grainKey, grainWithKey, useGlobalUpdater, useGlobalValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))

newtype Item = Item
  { name :: String
  , clicked :: Boolean
  }

instance grainItem :: Grain Item where
  initialState proxy =
    pure $ Item
      { name: "Globally stored Item" <> grainKey proxy
      , clicked: false
      }
  typeRefOf _ = fromConstructor Item

view :: Int -> VNode
view id =
  H.key (show id) $ H.fingerprint (show id) $ H.component do
    -- Store each item states globally.
    Item item <- useGlobalValue (grainWithKey $ show id :: _ Item)
    updateItem <- useGlobalUpdater (grainWithKey $ show id :: _ Item)
    updateItemIds <- useGlobalUpdater (grain :: _ ItemIds)

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
