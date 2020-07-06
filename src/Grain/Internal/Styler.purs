module Grain.Internal.Styler
  ( Styler
  , mountStyler
  , registerStyle
  ) where

import Prelude

import Data.Array (foldr)
import Data.Char (toCharCode)
import Data.Function.Uncurried as Fn
import Data.Int (base36, toStringAs)
import Data.Int.Bits (xor, zshr)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, trim)
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Uncurried as EFn
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (appendChild, createElement, head, setTextContent, whenE)
import Web.DOM.Element as E
import Web.DOM.Node (Node)

newtype Styler = Styler
  { node :: Node
  , stylesRef :: MObject String
  }

mountStyler :: Effect Styler
mountStyler = do
  node <- createStyleNode
  stylesRef <- MO.new
  pure $ Styler { node, stylesRef }

registerStyle :: EFn.EffectFn2 String Styler String
registerStyle = EFn.mkEffectFn2 \style (Styler s) -> do
  let minified = minify style
      name = "g" <> generateHash minified
  exists <- EFn.runEffectFn2 MO.has name s.stylesRef
  EFn.runEffectFn2 whenE (not exists) do
    let output = Fn.runFn2 replaceToken name minified
    EFn.runEffectFn3 MO.set name output s.stylesRef
    vs <- EFn.runEffectFn1 MO.values s.stylesRef
    EFn.runEffectFn2 setTextContent (joinWith "" vs) s.node
  pure name

replaceToken :: Fn.Fn2 String String String
replaceToken = Fn.mkFn2 \instead target ->
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
  h <- head
  el <- EFn.runEffectFn1 createElement "style"
  EFn.runEffectFn2 appendChild (E.toNode el) h
