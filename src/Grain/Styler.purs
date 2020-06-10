module Grain.Styler
  ( Styler
  , mountStyler
  , unmountStyler
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
import Effect.Ref (Ref, modify, new, read)
import Foreign.Object (Object, empty, insert, member, values)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, removeChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (head, toDocument)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Window (document)

newtype Styler = Styler
  { node :: Node
  , stylesRef :: Ref (Object String)
  }

mountStyler :: Effect Styler
mountStyler = do
  node <- createStyleNode
  stylesRef <- new empty
  pure $ Styler { node, stylesRef }

unmountStyler :: Styler -> Effect Unit
unmountStyler (Styler s) =
  void $ getHead >>= removeChild s.node

registerStyle :: String -> Styler -> Effect String
registerStyle style (Styler s) = do
  let minified = minify style
      name = "g" <> generateHash minified
  styles <- read s.stylesRef
  unless (member name styles) do
    let output = replaceToken name minified
    nextStyles <- joinWith "" <<< values <$> modify (insert name output) s.stylesRef
    setTextContent nextStyles s.node
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
