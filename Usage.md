# Usage

This document explains the usage of `purescript-grain`.

## Table of contents

- [How to construct UI](#how-to-construct-ui)
  - [Text](#text)
  - [Element](#element)
  - [Component](#component)
    - [State](#state)
    - [Portal](#portal)
- [Key](#key)
- [Fingerprint](#fingerprint)
- [Examples](#examples)
- [Module documentation](#module-documentation)

## How to construct UI

You can implement views with `VNode`.

There are 3 kinds as `VNode`.

- Text
  - Just a text node.
- Element
  - A element like `div`, `p` and so on.
- Component
  - A monad that you can declare that you use some states and updaters.

### Text

You can construct a text node with `text` function.

```purescript
import Grain (VNode)
import Grain.Markup as H

view :: VNode
view = H.text "Sample Text"
```

### Element

#### Tag

##### Basic

You can construct any element with tag helpers.

`Grain.Markup` module has many tag helpers, so you can implement views by using them basically.

The `div` example is below:

```purescript
import Grain (VNode)
import Grain.Markup as H

view :: VNode
view = H.div
```

##### If you can't find any helper you want

If you can't find any helper you want, you can use `element` function.

```purescript
import Grain (VNode)
import Grain.Markup as H

view :: VNode
view = H.element "svg"
```

#### Children

You can set child elements to an element with `kids` function.

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: VNode
view =
  H.ul # H.kids
    [ H.li # H.kids [ H.text "List Item 1" ]
    , H.li # H.kids [ H.text "List Item 2" ]
    ]
```

#### Property

##### Basic

You can set properties to an element with property helpers.

`Grain.Markup` module has many property helpers, so you can implement views by using them basically.

The `href` example is below:

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: VNode
view =
  H.a
    # H.href "https://github.com"
    # H.kids [ H.text "GitHub" ]
```

##### CSS

`purescript-grain` has the mechanism **CSS in PS**.

How to use:

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: VNode
view =
  H.div # H.css styles
  where
    styles =
      """
      .& {
        width: 100%;
        height: 100%;
        animation: & 2s infinite linear;
      }
      @keyframes & {
        from { width: 30%; } 
        to { width: 100%; }
      }
      """
```

You can write CSS passing CSS string to `css` function.

The mechanism is very simple.

`purescript-grain` emulates scoped CSS by generating hash from passed CSS string, and it is used as `class`, and it replaces `&` in CSS string with generated hash, then outputs styles.

It evaluates styles gradually as each node is rendered because styles evaluation is incorporated in rendering process.

##### If you can't find any helper you want

If you can't find any helper you want, you can use `prop` function.

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: VNode
view = H.element "svg" # H.prop "viewBox" "0 0 400 400"
```

#### Event handler

You can bind event handlers to an element with helper functions.

`Grain.Markup` module has many helpers, so you can implement views by using them.

The `onClick` example is below:

```purescript
import Prelude

import Effect.Console (log)
import Grain (VNode)
import Grain.Markup as H
import Web.Event.Event (stopPropagation)

view :: VNode
view =
  H.div # H.onClick onClick
  where
    onClick evt = do
      stopPropagation evt
      log "Clicked !!!"
```

#### Lifecycle

You can bind lifecycle handlers to an element.

- `didCreate`
  - Trigger when created a node
- `didUpdate`
  - Trigger when updated a node
- `didDelete`
  - Trigger when deleted a node

```purescript
import Prelude

import Effect.Console (log)
import Grain (VNode)
import Grain.Markup as H
import Web.DOM.Element (tagName)

view :: VNode
view =
  H.div
    # H.didCreate didCreate
    # H.didUpdate didUpdate
    # H.didDelete didDelete
  where
    didCreate el =
      log $ (tagName el) <> " created !!!"
    didUpdate el =
      log $ (tagName el) <> " updated !!!"
    didDelete el =
      log $ (tagName el) <> " deleted !!!"
```

### Component

You can construct a component with `component` function.

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: VNode
view = H.component do
  pure $ H.div # H.kids [ H.text "Sample Text" ]
```

#### State

In a component, you can declare that you use state with API like React Hooks.

- `useValue`
  - Listen a partial state, then return it.
  - If the partial state is changed, the component will be rerendered.
- `useUpdater`
  - Get an updater of a partial state.
- `useFinder`
  - Get a finder of a partial state.
  - This is useful when you want a state in a event handler only.

And **you can use some typeclasses to declare state, where you want, and at any level of granularity**.

##### Local state example

If you want to use local state, you can create a instance of `LocalGrain`.
It needs `initialState` and `typeRefOf`.

`TypeRef` is used as state key of store.
You can construct `TypeRef` with `fromConstructor` and any constructor function of a your state type.

Then you can call `useValue` or `useUpdater` with `LProxy` with a your state type.

```purescript
import Prelude

import Grain (class LocalGrain, LProxy(..), VNode, fromConstructor, useUpdater, useValue)
import Grain.Markup as H

newtype Count = Count Int

instance localGrainCount :: LocalGrain Count where
  initialState _ = pure $ Count 0
  typeRefOf _ = fromConstructor Count

view :: VNode
view = H.component do
  Count count <- useValue (LProxy :: _ Count)
  updateCount <- useUpdater (LProxy :: _ Count)
  let increment = updateCount (\(Count c) -> Count $ c + 1)
  pure $ H.div
    # H.onClick (const increment)
    # H.kids [ H.text $ show count ]
```

##### Global state example

If you want to use global state, you can create a instance of `GlobalGrain`.
It needs `initialState` and `typeRefOf`.

`TypeRef` is used as state key of store.
You can construct `TypeRef` with `fromConstructor` and any constructor function of a your state type.

Then you can call `useValue` or `useUpdater` with `GProxy` with a your state type.

State of `GlobalGrain` is registered in global store, therefore, the state isn't deleted when component is deleted, and you can get/update state regardless of component layers.

```purescript
import Prelude

import Grain (class GlobalGrain, GProxy(..), VNode, fromConstructor, useUpdater, useValue)
import Grain.Markup as H

newtype Count = Count Int

instance globalGrainCount :: GlobalGrain Count where
  initialState _ = pure $ Count 0
  typeRefOf _ = fromConstructor Count

view :: VNode
view = H.component do
  Count count <- useValue (GProxy :: _ Count)
  updateCount <- useUpdater (GProxy :: _ Count)
  let increment = updateCount (\(Count c) -> Count $ c + 1)
  pure $ H.div
    # H.onClick (const increment)
    # H.kids [ H.text $ show count ]
```

##### Keyed global state example

If you want to manage global state for each dynamic items, you can create a instance of `KeyedGlobalGrain`.
It needs `initialState` and `typeRefOf`.

`TypeRef` and a key of `KGProxy` are used as state key of store.
You can construct `TypeRef` with `fromConstructor` and any constructor function of a your state type.

Then you can call `useValue` or `useUpdater` with `KGProxy` with a key and a your state type.

State of `KeyedGlobalGrain` is registered in global store, therefore, the state isn't deleted when component is deleted, and you can get/update state regardless of component layers.

```purescript
import Prelude

import Grain (class KeyedGlobalGrain, KGProxy(..), VNode, fromConstructor, useUpdater, useValue)
import Grain.Markup as H

newtype Item = Item
  { name :: String
  , clicked :: Boolean
  }

instance keyedGlobalGrainItem :: KeyedGlobalGrain Item where
  initialState (KGProxy key) = pure $ Item
    { name: "Item " <> key
    , clicked: false
    }
  typeRefOf _ = fromConstructor Item

view :: String -> VNode
view key = H.component do
  Item item <- useValue (KGProxy key :: _ Item)
  updateItem <- useUpdater (KGProxy key :: _ Item)
  let onClick = updateItem (\(Item i) -> Item $ i { clicked = true })
  pure $ H.div
    # H.onClick (const onClick)
    # H.kids [ H.text $ item.name <> if item.clicked then " clicked" else "" ]
```

#### Portal

Portal is a way to render children into a node that exists outside the DOM hierarchy of the parent node.

You may want this when you implement dialog, dropdown and so on.

In that case, You can use `usePortal`.

`usePortal` is received parent node getter, then returns `VNode -> VNode`.

The function is received a child, and render it in parent node.

```purescript
import Prelude

import Data.Maybe (fromJust)
import Grain (VNode, usePortal)
import Grain.Markup as H
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (toNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

portalSample :: VNode
portalSample = H.component do
  portal <- usePortal do
    maybeEl <- window
      >>= document
      <#> toNonElementParentNode
      >>= getElementById "portal-root"
    -- NOTE: This element is written already in index.html
    pure $ toNode $ unsafePartial $ fromJust maybeEl

  pure $ portal $ H.div # H.kids [ H.text "sample" ]
```

## Key

**This section is very important.**

Key helps `purescript-grain` identify which child nodes have changed, are added, or are removed.

**Keys must be unique among siblings.**

You can add a key to an element or a component with `key` function.

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: Array Int -> VNode
view xs =
  H.ul # H.kids (listItem <$> xs)

listItem :: Int -> VNode
listItem x =
  H.li
    # H.key (show x)
    # H.kids [ H.text $ show x ]
```

You can write codes without keys, **but `purescript-grain` recommends using keys when map `Array a` to `Array VNode`**.

For your reference, if you do not add keys, `purescript-grain` generates key from node kind, tag name and index of array.

`purescript-grain` indentify `VNode` by keys, lifecycle mechanism is supported by it.

If Lifecycle is not triggered correctly, you should consider using keys.

## Fingerprint

For performance, you can use `fingerprint` function to skip rendering an element or a component.

**If fingerprint is same as previous rendered node's one, `purescript-grain` skips rendering process.**

```purescript
import Prelude

import Grain (VNode)
import Grain.Markup as H

view :: Int -> VNode
view count =
  H.div
    # H.fingerprint (show count)
    # H.kids [ H.text $ show count ]
```

## Examples

[examples](https://github.com/purescript-grain/purescript-grain/tree/master/examples)

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-grain).
