module Main where

import Prelude

import Data.DateTime (DateTime)
import Data.JSDate (fromDateTime, toString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import Grain (class Grain, VNode, fromConstructor, mountUI, useGlobalUpdater, useGlobalValue)
import Grain.Markup as H
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#app")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      void $ mountUI view $ toNode el

data Now = Now

instance showNow :: Show Now where
  show _ = "Now"

instance grainNow :: Grain Now DateTime where
  initialState _ = nowDateTime
  typeRefOf _ = fromConstructor Now

view :: VNode
view = H.component do
  updateNow <- useGlobalUpdater Now

  let listenNow = void $ setInterval 5000 do
        now <- nowDateTime
        updateNow $ const now

  pure $ H.div # H.didCreate (const listenNow) # H.kids
    [ nowView
    ]

nowView :: VNode
nowView = H.component do
  now <- useGlobalValue Now

  pure $ H.span # H.kids
    [ H.text $ toString $ fromDateTime now
    ]
