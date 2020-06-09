module Grain.UI.Element
  ( allocElement
  , updateElement
  ) where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import Grain.Styler (Styler)
import Grain.UI.Handler (allocHandlers, updateHandlers)
import Grain.UI.Prop (allocProps, updateProps)
import Grain.UI.Util (createElementNS_, createElement_)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)

type VElementPart r =
  { tagName :: String
  , props :: Object String
  , handlers :: Object (Event -> Effect Unit)
  | r
  }

allocElement
  :: forall r
   . Styler
  -> Boolean
  -> VElementPart r
  -> Effect Element
allocElement styler isSvg { tagName, props, handlers } = do
  el <- if isSvg
    then createElementNS_ tagName
    else createElement_ tagName
  allocProps styler isSvg props el
  allocHandlers handlers el
  pure el

updateElement
  :: forall r
   . Styler
  -> Boolean
  -> VElementPart r
  -> VElementPart r
  -> Element
  -> Effect Unit
updateElement styler isSvg current next el = do
  updateProps styler isSvg current.props next.props el
  updateHandlers current.handlers next.handlers el
