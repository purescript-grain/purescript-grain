module Grain.Markup.Prop where

import Prelude

import Data.String.CodeUnits (singleton)
import Data.String.Common (joinWith)
import Grain.UI (VNode, className, prop)

style :: String -> VNode -> VNode
style = prop "style"

classNames :: Array String -> VNode -> VNode
classNames = joinWith " " >>> className

id :: String -> VNode -> VNode
id = prop "id"

title :: String -> VNode -> VNode
title = prop "title"

hidden :: Boolean -> VNode -> VNode
hidden = show >>> prop "hidden"

type_ :: String -> VNode -> VNode
type_ = prop "type"

value :: String -> VNode -> VNode
value = prop "value"

defaultValue :: String -> VNode -> VNode
defaultValue = prop "defaultValue"

checked :: Boolean -> VNode -> VNode
checked = show >>> prop "checked"

placeholder :: String -> VNode -> VNode
placeholder = prop "placeholder"

selected :: Boolean -> VNode -> VNode
selected = show >>> prop "selected"

accept :: String -> VNode -> VNode
accept = prop "accept"

acceptCharset :: String -> VNode -> VNode
acceptCharset = prop "acceptCharset"

action :: String -> VNode -> VNode
action = prop "action"

autocomplete :: String -> VNode -> VNode
autocomplete = prop "autocomplete"

autofocus :: Boolean -> VNode -> VNode
autofocus = show >>> prop "autofocus"

disabled :: Boolean -> VNode -> VNode
disabled = show >>> prop "disabled"

enctype :: String -> VNode -> VNode
enctype = prop "enctype"

formAction :: String -> VNode -> VNode
formAction = prop "formAction"

list :: String -> VNode -> VNode
list = prop "list"

maxLength :: Int -> VNode -> VNode
maxLength = show >>> prop "maxLength"

minLength :: Int -> VNode -> VNode
minLength = show >>> prop "minLength"

method :: String -> VNode -> VNode
method = prop "method"

multiple :: Boolean -> VNode -> VNode
multiple = show >>> prop "multiple"

name :: String -> VNode -> VNode
name = prop "name"

noValidate :: Boolean -> VNode -> VNode
noValidate = show >>> prop "noValidate"

pattern :: String -> VNode -> VNode
pattern = prop "pattern"

readOnly :: Boolean -> VNode -> VNode
readOnly = show >>> prop "readOnly"

required :: Boolean -> VNode -> VNode
required = show >>> prop "required"

size :: Int -> VNode -> VNode
size = show >>> prop "size"

htmlFor :: String -> VNode -> VNode
htmlFor = prop "htmlFor"

form :: String -> VNode -> VNode
form = prop "form"

max :: String -> VNode -> VNode
max = prop "max"

min :: String -> VNode -> VNode
min = prop "min"

step :: String -> VNode -> VNode
step = prop "step"

cols :: Int -> VNode -> VNode
cols = show >>> prop "cols"

rows :: Int -> VNode -> VNode
rows = show >>> prop "rows"

wrap :: String -> VNode -> VNode
wrap = prop "wrap"

href :: String -> VNode -> VNode
href = prop "href"

target :: String -> VNode -> VNode
target = prop "target"

download :: String -> VNode -> VNode
download = prop "download"

hreflang :: String -> VNode -> VNode
hreflang = prop "hreflang"

media :: String -> VNode -> VNode
media = prop "media"

ping :: String -> VNode -> VNode
ping = prop "ping"

rel :: String -> VNode -> VNode
rel = prop "rel"

isMap :: Boolean -> VNode -> VNode
isMap = show >>> prop "isMap"

useMap :: String -> VNode -> VNode
useMap = prop "useMap"

shape :: String -> VNode -> VNode
shape = prop "shape"

coords :: String -> VNode -> VNode
coords = prop "coords"

src :: String -> VNode -> VNode
src = prop "src"

height :: Int -> VNode -> VNode
height = show >>> prop "height"

width :: Int -> VNode -> VNode
width = show >>> prop "width"

alt :: String -> VNode -> VNode
alt = prop "alt"

autoplay :: Boolean -> VNode -> VNode
autoplay = show >>> prop "autoplay"

controls :: Boolean -> VNode -> VNode
controls = show >>> prop "controls"

loop :: Boolean -> VNode -> VNode
loop = show >>> prop "loop"

preload :: String -> VNode -> VNode
preload = prop "preload"

poster :: String -> VNode -> VNode
poster = prop "poster"

default :: Boolean -> VNode -> VNode
default = show >>> prop "default"

kind_ :: String -> VNode -> VNode
kind_ = prop "kind"

srclang :: String -> VNode -> VNode
srclang = prop "srclang"

sandbox :: String -> VNode -> VNode
sandbox = prop "sandbox"

srcdoc :: String -> VNode -> VNode
srcdoc = prop "srcdoc"

reversed :: Boolean -> VNode -> VNode
reversed = show >>> prop "reversed"

start :: Int -> VNode -> VNode
start = show >>> prop "start"

colSpan :: Int -> VNode -> VNode
colSpan = show >>> prop "colSpan"

rowSpan :: Int -> VNode -> VNode
rowSpan = show >>> prop "rowSpan"

headers :: String -> VNode -> VNode
headers = prop "headers"

scope :: String -> VNode -> VNode
scope = prop "scope"

accessKey :: Char -> VNode -> VNode
accessKey = singleton >>> prop "accessKey"

contentEditable :: Boolean -> VNode -> VNode
contentEditable = show >>> prop "contentEditable"

dir :: String -> VNode -> VNode
dir = prop "dir"

draggable :: Boolean -> VNode -> VNode
draggable = show >>> prop "draggable"

dropzone :: String -> VNode -> VNode
dropzone = prop "dropzone"

lang :: String -> VNode -> VNode
lang = prop "lang"

spellcheck :: Boolean -> VNode -> VNode
spellcheck = show >>> prop "spellcheck"

tabIndex :: Int -> VNode -> VNode
tabIndex = show >>> prop "tabIndex"

cite :: String -> VNode -> VNode
cite = prop "cite"

dateTime :: String -> VNode -> VNode
dateTime = prop "dateTime"
