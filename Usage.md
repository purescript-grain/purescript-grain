# Usage

This document explains the usage of `purescript-grain`.

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

Coming soon.

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
