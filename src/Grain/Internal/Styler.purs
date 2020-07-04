module Grain.Internal.Styler
  ( Styler
  , mountStyler
  , registerStyle
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.Int (base36, toStringAs)
import Data.Int.Bits (xor, zshr)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (head, toDocument)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)

newtype Styler = Styler
  { node :: Node
  , stylesRef :: MObject String
  }

mountStyler :: Effect Styler
mountStyler = do
  node <- createStyleNode
  stylesRef <- MO.new
  pure $ Styler { node, stylesRef }

registerStyle :: String -> Styler -> Effect String
registerStyle style (Styler s) = do
  let minified = minify style
      name = "g" <> generateHash minified
  exists <- MO.has name s.stylesRef
  unless exists do
    let output = replaceToken name minified
    MO.set name output s.stylesRef
    vs <- MO.values s.stylesRef
    setTextContent (joinWith "" vs) s.node
  pure name

replaceToken :: String -> String -> String
replaceToken instead target =
  replaceAll (Pattern "&") (Replacement instead) target

minify :: String -> String
minify = trim >>> replaceReturns >>> replaceWhitespaces
  where
    replaceReturns = replaceAll (Pattern "\n") (Replacement "")
    replaceWhitespaces = replace (unsafeRegex "\\s\\s+" global) " "

generateHash :: String -> String
generateHash str = toStringAs base36 $ zshr seed 0
  where
    culc char value = xor (value * 33) (toCharCode char)
    seed = foldr culc 5381 $ toCharArray str

createStyleNode :: Effect Node
createStyleNode = do
  el <- window >>= document <#> toDocument >>= createElement "style"
  getHead >>= appendChild (E.toNode el)

getHead :: Effect Node
getHead =
  unsafePartial $ fromJust <$> (window >>= document >>= head) <#> toNode
