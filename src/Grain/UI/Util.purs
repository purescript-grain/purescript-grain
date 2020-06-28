module Grain.UI.Util
  ( raf
  , childNode
  , putNode
  , createText_
  , createElement_
  , createElementNS_
  , setAttributeNS_
  , removeAttributeNS_
  , hasXlinkPrefix
  , setAny
  , isProperty
  , isBoolean
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, replace, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4, runEffectFn3, runEffectFn4)
import Web.DOM.Document (Document, createElement, createElementNS, createTextNode)
import Web.DOM.Element (Element)
import Web.DOM.Node (Node, appendChild, childNodes, insertBefore)
import Web.DOM.NodeList (item)
import Web.DOM.Text (Text)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, requestAnimationFrame)

raf :: Effect Unit -> Effect Unit
raf f = void $ window >>= requestAnimationFrame f

childNode :: Int -> Node -> Effect (Maybe Node)
childNode i node = childNodes node >>= item i

putNode :: Int -> Node -> Node -> Effect Node
putNode i child parent = do
  maybeTarget <- childNode i parent
  case maybeTarget of
    Nothing -> appendChild child parent
    Just target -> insertBefore child target parent

createText_ :: String -> Effect Text
createText_ text = doc >>= createTextNode text

createElement_ :: String -> Effect Element
createElement_ tagName =
  doc >>= createElement tagName

createElementNS_ :: String -> Effect Element
createElementNS_ tagName =
  doc >>= createElementNS svgNameSpace tagName

setAttributeNS_ :: String -> String -> Element -> Effect Unit
setAttributeNS_ name val el =
  setAttributeNS xlinkNameSpace (replace xlinkRegex "" name) val el

removeAttributeNS_ :: String -> Element -> Effect Unit
removeAttributeNS_ name el =
  removeAttributeNS xlinkNameSpace (replace xlinkRegex "" name) el

hasXlinkPrefix :: String -> Boolean
hasXlinkPrefix = test xlinkRegex

xlinkRegex :: Regex
xlinkRegex = unsafeRegex "^xlink:" noFlags

doc :: Effect Document
doc = window >>= document <#> toDocument

svgNameSpace :: Maybe String
svgNameSpace = Just "http://www.w3.org/2000/svg"

xlinkNameSpace :: String
xlinkNameSpace = "http://www.w3.org/1999/xlink"

setAny :: forall a. String -> a -> Element -> Effect Unit
setAny name x element =
  runEffectFn3 setAnyImpl name x element

setAttributeNS :: String -> String -> String -> Element -> Effect Unit
setAttributeNS ns name val element =
  runEffectFn4 setAttributeNSImpl ns name val element

removeAttributeNS :: String -> String -> Element -> Effect Unit
removeAttributeNS ns name element =
  runEffectFn3 removeAttributeNSImpl ns name element

isProperty :: String -> Element -> Boolean
isProperty name element =
  runFn2 isPropertyImpl name element

isBoolean :: String -> Element -> Boolean
isBoolean name element =
  runFn2 isBooleanImpl name element

foreign import setAnyImpl :: forall a. EffectFn3 String a Element Unit
foreign import setAttributeNSImpl :: EffectFn4 String String String Element Unit
foreign import removeAttributeNSImpl :: EffectFn3 String String Element Unit
foreign import isPropertyImpl :: Fn2 String Element Boolean
foreign import isBooleanImpl :: Fn2 String Element Boolean
