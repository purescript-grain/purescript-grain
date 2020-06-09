module Grain.Markup.Handler where

import Prelude

import Effect (Effect)
import Grain.UI (VNode, handle)
import Web.Event.Event (Event)

onAbort :: (Event -> Effect Unit) -> VNode -> VNode
onAbort = handle "onabort"

onBlur :: (Event -> Effect Unit) -> VNode -> VNode
onBlur = handle "onblur"

onCancel :: (Event -> Effect Unit) -> VNode -> VNode
onCancel = handle "oncancel"

onCanPlay :: (Event -> Effect Unit) -> VNode -> VNode
onCanPlay = handle "oncanplay"

onCanPlayThrough :: (Event -> Effect Unit) -> VNode -> VNode
onCanPlayThrough = handle "oncanplaythrough"

onChange :: (Event -> Effect Unit) -> VNode -> VNode
onChange = handle "onchange"

onClick :: (Event -> Effect Unit) -> VNode -> VNode
onClick = handle "onclick"

onContextMenu :: (Event -> Effect Unit) -> VNode -> VNode
onContextMenu = handle "oncontextmenu"

onCueChange :: (Event -> Effect Unit) -> VNode -> VNode
onCueChange = handle "oncuechange"

onDoubleClick :: (Event -> Effect Unit) -> VNode -> VNode
onDoubleClick = handle "ondblclick"

onDrag :: (Event -> Effect Unit) -> VNode -> VNode
onDrag = handle "ondrag"

onDragEnd :: (Event -> Effect Unit) -> VNode -> VNode
onDragEnd = handle "ondragend"

onDragEnter :: (Event -> Effect Unit) -> VNode -> VNode
onDragEnter = handle "ondragenter"

onDragLeave :: (Event -> Effect Unit) -> VNode -> VNode
onDragLeave = handle "ondragleave"

onDragOver :: (Event -> Effect Unit) -> VNode -> VNode
onDragOver = handle "ondragover"

onDragStart :: (Event -> Effect Unit) -> VNode -> VNode
onDragStart = handle "ondragstart"

onDrop :: (Event -> Effect Unit) -> VNode -> VNode
onDrop = handle "ondrop"

onDurationChange :: (Event -> Effect Unit) -> VNode -> VNode
onDurationChange = handle "ondurationchange"

onEmptied :: (Event -> Effect Unit) -> VNode -> VNode
onEmptied = handle "onemptied"

onEnded :: (Event -> Effect Unit) -> VNode -> VNode
onEnded = handle "onended"

onError :: (Event -> Effect Unit) -> VNode -> VNode
onError = handle "onerror"

onFocus :: (Event -> Effect Unit) -> VNode -> VNode
onFocus = handle "onfocus"

onInput :: (Event -> Effect Unit) -> VNode -> VNode
onInput = handle "oninput"

onInvalid :: (Event -> Effect Unit) -> VNode -> VNode
onInvalid = handle "oninvalid"

onKeyDown :: (Event -> Effect Unit) -> VNode -> VNode
onKeyDown = handle "onkeydown"

onKeyPress :: (Event -> Effect Unit) -> VNode -> VNode
onKeyPress = handle "onkeypress"

onKeyUp :: (Event -> Effect Unit) -> VNode -> VNode
onKeyUp = handle "onkeyup"

onLoad :: (Event -> Effect Unit) -> VNode -> VNode
onLoad = handle "onload"

onLoadedData :: (Event -> Effect Unit) -> VNode -> VNode
onLoadedData = handle "onloadeddata"

onLoadedMetadata :: (Event -> Effect Unit) -> VNode -> VNode
onLoadedMetadata = handle "onloadedmetadata"

onLoadStart :: (Event -> Effect Unit) -> VNode -> VNode
onLoadStart = handle "onloadstart"

onMouseDown :: (Event -> Effect Unit) -> VNode -> VNode
onMouseDown = handle "onmousedown"

onMouseEnter :: (Event -> Effect Unit) -> VNode -> VNode
onMouseEnter = handle "onmouseenter"

onMouseLeave :: (Event -> Effect Unit) -> VNode -> VNode
onMouseLeave = handle "onmouseleave"

onMouseMove :: (Event -> Effect Unit) -> VNode -> VNode
onMouseMove = handle "onmousemove"

onMouseOut :: (Event -> Effect Unit) -> VNode -> VNode
onMouseOut = handle "onmouseout"

onMouseOver :: (Event -> Effect Unit) -> VNode -> VNode
onMouseOver = handle "onmouseover"

onMouseUp :: (Event -> Effect Unit) -> VNode -> VNode
onMouseUp = handle "onmouseup"

onPause :: (Event -> Effect Unit) -> VNode -> VNode
onPause = handle "onpause"

onPlay :: (Event -> Effect Unit) -> VNode -> VNode
onPlay = handle "onplay"

onPlaying :: (Event -> Effect Unit) -> VNode -> VNode
onPlaying = handle "onplaying"

onProgress :: (Event -> Effect Unit) -> VNode -> VNode
onProgress = handle "onprogress"

onRateChange :: (Event -> Effect Unit) -> VNode -> VNode
onRateChange = handle "onratechange"

onReset :: (Event -> Effect Unit) -> VNode -> VNode
onReset = handle "onreset"

onScroll :: (Event -> Effect Unit) -> VNode -> VNode
onScroll = handle "onscroll"

onSeeked :: (Event -> Effect Unit) -> VNode -> VNode
onSeeked = handle "onseeked"

onSeeking :: (Event -> Effect Unit) -> VNode -> VNode
onSeeking = handle "onseeking"

onSelect :: (Event -> Effect Unit) -> VNode -> VNode
onSelect = handle "onselect"

onStalled :: (Event -> Effect Unit) -> VNode -> VNode
onStalled = handle "onstalled"

onSubmit :: (Event -> Effect Unit) -> VNode -> VNode
onSubmit = handle "onsubmit"

onSuspend :: (Event -> Effect Unit) -> VNode -> VNode
onSuspend = handle "onsuspend"

onTimeUpdate :: (Event -> Effect Unit) -> VNode -> VNode
onTimeUpdate = handle "ontimeupdate"

onToggle :: (Event -> Effect Unit) -> VNode -> VNode
onToggle = handle "ontoggle"

onVolumeChange :: (Event -> Effect Unit) -> VNode -> VNode
onVolumeChange = handle "onvolumechange"

onWaiting :: (Event -> Effect Unit) -> VNode -> VNode
onWaiting = handle "onwaiting"

onWheel :: (Event -> Effect Unit) -> VNode -> VNode
onWheel = handle "onwheel"

onPointerDown :: (Event -> Effect Unit) -> VNode -> VNode
onPointerDown = handle "onpointerdown"

onPointerMove :: (Event -> Effect Unit) -> VNode -> VNode
onPointerMove = handle "onpointermove"

onPointerUp :: (Event -> Effect Unit) -> VNode -> VNode
onPointerUp = handle "onpointerup"

onPointerCancel :: (Event -> Effect Unit) -> VNode -> VNode
onPointerCancel = handle "onpointercancel"

onPointerOver :: (Event -> Effect Unit) -> VNode -> VNode
onPointerOver = handle "onpointerover"

onPointerOut :: (Event -> Effect Unit) -> VNode -> VNode
onPointerOut = handle "onpointerout"

onPointerEnter :: (Event -> Effect Unit) -> VNode -> VNode
onPointerEnter = handle "onpointerenter"

onPointerLeave :: (Event -> Effect Unit) -> VNode -> VNode
onPointerLeave = handle "onpointerleave"
