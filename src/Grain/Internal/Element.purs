module Grain.Internal.Element
  ( allocElement
  , updateElement
  ) where

import Prelude

import Effect.Uncurried as EFn
import Grain.Internal.Handler (Handlers, allocHandlers, updateHandlers)
import Grain.Internal.Prop (Props, allocProps, updateProps)
import Grain.Internal.SpecialProp (SpecialProps, allocSpecialProps, updateSpecialProps)
import Grain.Internal.Styler (Styler)
import Grain.Internal.Util (createElementNS, createElement)
import Web.DOM.Element (Element)

type VElementPart r =
  { tagName :: String
  , props :: Props
  , handlers :: Handlers
  , specialProps :: SpecialProps
  | r
  }

allocElement
  :: forall r
   . EFn.EffectFn3 Boolean Styler (VElementPart r) Element
allocElement = EFn.mkEffectFn3 \isSvg styler next -> do
  element <- if isSvg
    then EFn.runEffectFn1 createElementNS next.tagName
    else EFn.runEffectFn1 createElement next.tagName
  EFn.runEffectFn4 allocSpecialProps isSvg styler next.specialProps element
  EFn.runEffectFn3 allocProps isSvg next.props element
  EFn.runEffectFn2 allocHandlers next.handlers element
  pure element

updateElement
  :: forall r
   . EFn.EffectFn5 Boolean Styler (VElementPart r) (VElementPart r) Element Unit
updateElement = EFn.mkEffectFn5 \isSvg styler current next element -> do
  EFn.runEffectFn5 updateSpecialProps isSvg styler current.specialProps next.specialProps element
  EFn.runEffectFn4 updateProps isSvg current.props next.props element
  EFn.runEffectFn3 updateHandlers current.handlers next.handlers element
