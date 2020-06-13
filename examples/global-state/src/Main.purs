module Main where

import Prelude

import Data.DateTime (DateTime)
import Data.JSDate (fromDateTime, toString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import Grain (class Grain, VNode, fromConstructor, grain, mountUI, useGlobalUpdater, useGlobalValue)
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

newtype Now = Now DateTime

instance grainNow :: Grain Now where
  initialState _ = Now <$> nowDateTime
  typeRefOf _ = fromConstructor Now

view :: VNode
view = H.component do
  updateNow <- useGlobalUpdater (grain :: _ Now)

  let listenNow = void $ setInterval 5000 do
        now <- nowDateTime
        updateNow $ const $ Now now

  pure $ H.div # H.didCreate (const listenNow) # H.kids
    [ nowView
    ]

nowView :: VNode
nowView = H.component do
  Now now <- useGlobalValue (grain :: _ Now)

  pure $ H.span # H.kids
    [ H.text $ toString $ fromDateTime now
    ]
