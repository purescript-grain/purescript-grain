module Grain.UI
  ( VNode
  , key
  , fingerprint
  , component
  , element
  , text
  , kids
  , prop
  , handle
  , css
  , className
  , didCreate
  , didUpdate
  , didDelete
  , Render
  , Query
  , runRender
  , useValue
  , useFinder
  , useUpdater
  , usePortal
  , mount
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (snoc, (!!))
import Data.Function.Uncurried as Fn
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Uncurried as EFn
import Grain.Class (class Grain, which)
import Grain.Internal.Diff (Create, Delete, GetKey, Move, Update, Patch, diff)
import Grain.Internal.Element (allocElement, updateElement)
import Grain.Internal.Handler (Handlers)
import Grain.Internal.MArray (MArray)
import Grain.Internal.MArray as MA
import Grain.Internal.MMap (MMap)
import Grain.Internal.MMap as MM
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Prop (Props)
import Grain.Internal.Ref (Ref)
import Grain.Internal.Ref as Ref
import Grain.Internal.SpecialProp (SpecialProps)
import Grain.Internal.Store (Store, createStore, readGrain, subscribeGrain, unsubscribeGrain, updateGrain)
import Grain.Internal.Styler (Styler, mountStyler)
import Grain.Internal.Util (byIdx, createTextNode, nodeIndexOf, putChild, raf, removeChild, sequenceE, setTextContent, whenE)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Web.DOM.Element as E
import Web.DOM.Node (Node)
import Web.DOM.Text as T
import Web.Event.Event (Event)



-- | The type of virtual node.
data VNode = VNode (Maybe String) VElement

data VElement
  = VText String
  | VElement
      { fingerprint :: Maybe String
      , tagName :: String
      , props :: Props
      , handlers :: Handlers
      , children :: Array VNode
      , specialProps :: SpecialProps
      , didCreate :: Maybe (Element -> Effect Unit)
      , didUpdate :: Maybe (Element -> Effect Unit)
      , didDelete :: Maybe (Element -> Effect Unit)
      }
  | VComponent
      { fingerprint :: Maybe String
      , render :: Render VNode
      }

-- | Add a key to a `VNode`.
key :: String -> VNode -> VNode
key k (VNode _ velement) =
  VNode (Just k) velement

-- | Add a fingerprint to check equality of a `VNode`.
-- |
-- | If it is same as previous rendered element's one, rendering will be skipped.
fingerprint :: String -> VNode -> VNode
fingerprint fp (VNode k (VComponent r)) =
  VNode k $ VComponent r { fingerprint = Just fp }
fingerprint fp (VNode k (VElement r)) =
  VNode k $ VElement r { fingerprint = Just fp }
fingerprint _ vnode = vnode

-- | Create a `VNode` of component.
component :: Render VNode -> VNode
component render = VNode Nothing $ VComponent
  { fingerprint: Nothing
  , render
  }

-- | Create a `VNode` of specified tag element.
element :: String -> VNode
element tagName = VNode Nothing $ VElement
  { fingerprint: Nothing
  , tagName
  , props: []
  , handlers: []
  , children: []
  , specialProps:
      { css: Nothing
      , className: Nothing
      }
  , didCreate: Nothing
  , didUpdate: Nothing
  , didDelete: Nothing
  }

-- | Create a `VNode` of text.
text :: String -> VNode
text = VNode Nothing <<< VText

-- | Add children.
kids :: Array VNode -> VNode -> VNode
kids children (VNode k (VElement r)) =
  VNode k $ VElement r { children = children }
kids _ velement = velement

-- | Add a property.
prop :: String -> String -> VNode -> VNode
prop name val (VNode k (VElement r)) =
  VNode k $ VElement r { props = snoc r.props (Tuple name val) }
prop _ _ velement = velement

-- | Bind an event handler.
handle
  :: String
  -> (Event -> Effect Unit)
  -> VNode
  -> VNode
handle name handler (VNode k (VElement r)) =
  VNode k $ VElement r { handlers = snoc r.handlers (Tuple name handler) }
handle _ _ velement = velement

-- | Define styles with CSS string.
-- |
-- | It generates a hash string as class name from CSS string, and the generated class name is used automatically.
-- |
-- | ```purescript
-- | justDiv :: VNode
-- | justDiv =
-- |   H.div # H.css styles
-- |
-- | styles :: String
-- | styles =
-- |   """
-- |   .& {
-- |     width: 100px;
-- |     height: 100px;
-- |   }
-- |   .&:hover {
-- |     width: 100px;
-- |     height: 100px;
-- |   }
-- |   .&:hover .selected {
-- |     color: blue;
-- |   }
-- |   """
-- | ```
-- |
-- | `&` in the CSS string is replaced with the generated class name, and output it as stylesheet.
-- |
-- | Like this:
-- |
-- | ```css
-- | .gz66dqm {
-- |   width: 100px;
-- |   height: 100px;
-- | }
-- | .gz66dqm:hover {
-- |   width: 100px;
-- |   height: 100px;
-- | }
-- | .gz66dqm:hover .selected {
-- |   color: blue;
-- | }
-- | ```
css :: String -> VNode -> VNode
css val (VNode k (VElement r)) =
  VNode k $ VElement r { specialProps { css = Just val } }
css _ velement = velement

className :: String -> VNode -> VNode
className val (VNode k (VElement r)) =
  VNode k $ VElement r { specialProps { className = Just val } }
className _ velement = velement

-- | Bind `didCreate` lifecycle.
didCreate
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didCreate handler (VNode k (VElement r)) =
  VNode k $ VElement r { didCreate = Just handler }
didCreate _ velement = velement

-- | Bind `didUpdate` lifecycle.
didUpdate
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didUpdate handler (VNode k (VElement r)) =
  VNode k $ VElement r { didUpdate = Just handler }
didUpdate _ velement = velement

-- | Bind `didDelete` lifecycle.
didDelete
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didDelete handler (VNode k (VElement r)) =
  VNode k $ VElement r { didDelete = Just handler }
didDelete _ velement = velement



-- | The type of component renderer.
-- |
-- | In this monad, you can declare that you use some states and updaters.
newtype Render a = Render (ReaderT QueryBox Effect a)

-- Do not derive MonadEffect
derive newtype instance functorRender :: Functor Render
derive newtype instance applyRender :: Apply Render
derive newtype instance applicativeRender :: Applicative Render
derive newtype instance bindRender :: Bind Render
derive newtype instance monadRender :: Monad Render
derive newtype instance semigroupRender :: Semigroup a => Semigroup (Render a)
derive newtype instance monoidRender :: Monoid a => Monoid (Render a)
derive newtype instance monadRecRender :: MonadRec Render

newtype QueryBox = QueryBox Query

type Query =
  { selectValue :: forall p a. Grain p a => p a -> Effect a
  , listenValue :: forall p a. Grain p a => p a -> Effect Unit
  , updateValue :: forall p a. Grain p a => p a -> (a -> a) -> Effect Unit
  , portalVNode :: Effect Node -> VNode -> VNode
  }

runRender :: forall a. Render a -> Query -> Effect a
runRender (Render reader) = runReaderT reader <<< QueryBox

-- | Listen a state, then return it.
-- |
-- | If the state is changed, the component will be rerendered.
useValue
  :: forall p a
   . Grain p a
  => p a
  -> Render a
useValue proxy = Render do
  QueryBox query <- ask
  liftEffect do
    query.listenValue proxy
    query.selectValue proxy

-- | Get a finder of a state.
useFinder
  :: forall p a
   . Grain p a
  => p a
  -> Render (Effect a)
useFinder proxy = Render do
  QueryBox query <- ask
  pure $ query.selectValue proxy

-- | Get an updater of a state.
useUpdater
  :: forall p a
   . Grain p a
  => p a
  -> Render ((a -> a) -> Effect Unit)
useUpdater proxy = Render do
  QueryBox query <- ask
  pure $ query.updateValue proxy

-- | Get portal function.
usePortal :: Effect Node -> Render (VNode -> VNode)
usePortal getPortalRoot = Render do
  QueryBox query <- ask
  pure $ query.portalVNode getPortalRoot



-- | Mount a `VNode` to a parent node.
mount :: VNode -> Node -> Effect Unit
mount vnode parentNode = do
  store <- createStore
  styler <- mountStyler
  componentRefs <- MM.new
  nodeRefs <- newNodeRefs
  EFn.runEffectFn2 registerParentNode parentNode nodeRefs
  EFn.runEffectFn2 diff (force patch)
    { context:
        { isSvg: false
        , deleting: false
        , store
        , styler
        , nodeRefs
        , componentRefs
        }
    , parentNode
    , currents: []
    , nexts: [ vnode ]
    }



type UIContext =
  { isSvg :: Boolean
  , deleting :: Boolean
  , store :: Store
  , styler :: Styler
  , nodeRefs :: NodeRefs
  , componentRefs :: MMap Node ComponentRef
  }

switchToSvg :: Fn.Fn2 String UIContext UIContext
switchToSvg = Fn.mkFn2 \tag context ->
  if context.isSvg
    then context
    else context { isSvg = tag == "svg" }

switchToDeleting :: UIContext -> UIContext
switchToDeleting context =
  if context.deleting
    then context
    else context { deleting = true }



-- Use lazy to avoid cyclic reference
patch :: Lazy (Patch UIContext Node VNode)
patch = defer \_ ->
  { getKey
  , create
  , delete
  , update
  , move
  }

getKey :: GetKey VNode
getKey = Fn.mkFn2 \idx (VNode k velement) ->
  let identifier =
        case k of
          Just k' -> k'
          _ -> show idx
   in case velement of
        VText _ ->
          "text_" <> identifier
        VElement { tagName } ->
          "element_" <> tagName <> "_" <> identifier
        VComponent _ ->
          "component_" <> identifier

create :: Create UIContext Node VNode
create = EFn.mkEffectFn5
  \context parentNode nodeKey index (VNode _ next) -> do
    node <- EFn.runEffectFn4 eval context Nothing Nothing (Just next)
    EFn.runEffectFn4 registerChildNode parentNode nodeKey node context.nodeRefs
    EFn.runEffectFn3 putChild index node parentNode

delete :: Delete UIContext Node VNode
delete = EFn.mkEffectFn4
  \context parentNode nodeKey (VNode _ current) -> do
    let ctx = switchToDeleting context
    target <- EFn.runEffectFn3 getChildNode parentNode nodeKey ctx.nodeRefs
    node <- EFn.runEffectFn4 eval ctx target (Just current) Nothing
    EFn.runEffectFn2 whenE (not context.deleting) do
      EFn.runEffectFn3 unregisterChildNode parentNode nodeKey ctx.nodeRefs
      EFn.runEffectFn2 removeChild node parentNode

update :: Update UIContext Node VNode
update = EFn.mkEffectFn5
  \context parentNode nodeKey (VNode _ current) (VNode _ next) -> do
    target <- EFn.runEffectFn3 getChildNode parentNode nodeKey context.nodeRefs
    void $ EFn.runEffectFn4 eval context target (Just current) (Just next)

move :: Move UIContext Node VNode
move = EFn.mkEffectFn6
  \context parentNode nodeKey index (VNode _ current) (VNode _ next) -> do
    target <- EFn.runEffectFn3 getChildNode parentNode nodeKey context.nodeRefs
    node <- EFn.runEffectFn4 eval context target (Just current) (Just next)
    ni <- EFn.runEffectFn1 nodeIndexOf node
    let adjustedIdx = if ni < index then index + 1 else index
    EFn.runEffectFn3 putChild adjustedIdx node parentNode



eval :: EFn.EffectFn4 UIContext (Maybe Node) (Maybe VElement) (Maybe VElement) Node
eval = EFn.mkEffectFn4 \context target current next -> do
  case target, current, next of
    -- Update
    Just node, Just (VText ct), Just (VText nt) -> do
      EFn.runEffectFn2 whenE (ct /= nt) do
        EFn.runEffectFn2 setTextContent nt node
      pure node
    Just node, Just (VComponent cc), Just (VComponent nc) -> do
      EFn.runEffectFn2 whenE (Fn.runFn2 isDifferent cc nc) do
        componentRef <- EFn.runEffectFn2 MM.unsafeGet node context.componentRefs
        void $ EFn.runEffectFn4 evalComponent context (Just node) nc.render componentRef
      pure node
    Just node, Just (VElement cv), Just (VElement nv) -> do
      EFn.runEffectFn2 whenE (Fn.runFn2 isDifferent cv nv) do
        let el = unsafeCoerce node
            ctx = Fn.runFn2 switchToSvg nv.tagName context
        EFn.runEffectFn5 updateElement ctx.isSvg ctx.styler cv nv el
        EFn.runEffectFn2 diff (force patch)
          { context: ctx
          , parentNode: node
          , currents: cv.children
          , nexts: nv.children
          }
        EFn.runEffectFn2 runLifecycle nv.didUpdate el
      pure node

    -- Create
    Nothing, Nothing, Just (VText nt) -> do
      txt <- EFn.runEffectFn1 createTextNode nt
      pure $ T.toNode txt
    Nothing, Nothing, Just (VComponent nc) -> do
      componentRef <- newComponentRef
      node <- EFn.runEffectFn4 evalComponent context Nothing nc.render componentRef
      EFn.runEffectFn3 MM.set node componentRef context.componentRefs
      pure node
    Nothing, Nothing, Just (VElement nv) -> do
      let ctx = Fn.runFn2 switchToSvg nv.tagName context
      el <- EFn.runEffectFn3 allocElement ctx.isSvg ctx.styler nv
      let node = E.toNode el
      EFn.runEffectFn2 registerParentNode node ctx.nodeRefs
      EFn.runEffectFn2 diff (force patch)
        { context: ctx
        , parentNode: node
        , currents: []
        , nexts: nv.children
        }
      EFn.runEffectFn2 runLifecycle nv.didCreate el
      pure node

    -- Delete
    Just node, Just (VText _), Nothing ->
      pure node
    Just node, Just (VComponent _), Nothing -> do
      componentRef <- EFn.runEffectFn2 MM.unsafeGet node context.componentRefs
      EFn.runEffectFn3 unmountComponent context (Just node) componentRef
      EFn.runEffectFn2 MM.del node context.componentRefs
      pure node
    Just node, Just (VElement cv), Nothing -> do
      EFn.runEffectFn2 diff (force patch)
        { context
        , parentNode: node
        , currents: cv.children
        , nexts: []
        }
      EFn.runEffectFn2 runLifecycle cv.didDelete $ unsafeCoerce node
      EFn.runEffectFn2 unregisterParentNode node context.nodeRefs
      pure node

    _, _, _ ->
      throw "Renderer can't evaluate vnodes."

isDifferent
  :: forall r
   . Fn.Fn2
       { fingerprint :: Maybe String | r }
       { fingerprint :: Maybe String | r }
       Boolean
isDifferent = Fn.mkFn2 \c n ->
  case c.fingerprint, n.fingerprint of
    Nothing, Nothing -> true
    Just cf, Just nf -> cf /= nf
    _, _ -> true

runLifecycle
  :: EFn.EffectFn2 (Maybe (Element -> Effect Unit)) Element Unit
runLifecycle = EFn.mkEffectFn2 \lifecycle el ->
  case lifecycle of
    Nothing -> pure unit
    Just l -> EFn.runEffectFn1 raf $ l el



type ComponentRef = Ref
  { rendering :: Boolean
  , unsubscribers :: MArray (Effect Unit)
  , history :: MArray VElement
  , store :: Store
  , portalHistory :: MArray VNode
  }

newComponentRef :: Effect ComponentRef
newComponentRef = do
  store <- createStore
  unsubscribers <- MA.new
  history <- MA.new
  portalHistory <- MA.new
  EFn.runEffectFn1 Ref.new
    { rendering: true
    , unsubscribers
    , history
    , store
    , portalHistory
    }

evalComponent :: EFn.EffectFn4 UIContext (Maybe Node) (Render VNode) ComponentRef Node
evalComponent = EFn.mkEffectFn4 \context target render componentRef -> do
  targetRef <- EFn.runEffectFn1 Ref.new target

  let storeSelection = do
        local <- EFn.runEffectFn1 componentStore componentRef
        pure { global: context.store, local }

      listenValue :: forall p a. Grain p a => p a -> Effect Unit
      listenValue proxy = do
        selection <- storeSelection
        let store = which proxy selection
        EFn.runEffectFn3 subscribeGrain proxy evaluateRaf store
        EFn.runEffectFn2 addComponentUnsubscriber
          (EFn.runEffectFn3 unsubscribeGrain proxy evaluateRaf store)
          componentRef

      selectValue :: forall p a. Grain p a => p a -> Effect a
      selectValue proxy = do
        selection <- storeSelection
        let store = which proxy selection
        EFn.runEffectFn2 readGrain proxy store

      updateValue :: forall p a. Grain p a => p a -> (a -> a) -> Effect Unit
      updateValue proxy f = do
        selection <- storeSelection
        let store = which proxy selection
        EFn.runEffectFn3 updateGrain proxy f store

      portalVNode = Fn.runFn2 getPortal context componentRef

      evaluate = do
        EFn.runEffectFn1 unlockRendering componentRef
        EFn.runEffectFn1 triggerUnsubscriber componentRef
        VNode _ velement <- runRender render
          { selectValue
          , listenValue
          , updateValue
          , portalVNode
          }
        h <- EFn.runEffectFn2 addComponentHistory velement componentRef
        t <- EFn.runEffectFn1 Ref.read targetRef
        EFn.runEffectFn4 eval context t (h !! 1) (h !! 0)

      evaluateRaf = do
        locked <- EFn.runEffectFn1 componentRenderingLock componentRef
        EFn.runEffectFn2 whenE (not locked) do
          EFn.runEffectFn1 lockRendering componentRef
          EFn.runEffectFn1 raf (void evaluate)

  node <- evaluate
  EFn.runEffectFn2 Ref.write (Just node) targetRef
  pure node

unmountComponent :: EFn.EffectFn3 UIContext (Maybe Node) ComponentRef Unit
unmountComponent = EFn.mkEffectFn3 \context target componentRef -> do
  EFn.runEffectFn1 triggerUnsubscriber componentRef
  h <- EFn.runEffectFn1 componentHistory componentRef
  void $ EFn.runEffectFn4 eval context target (h !! 0) Nothing

lockRendering :: EFn.EffectFn1 ComponentRef Unit
lockRendering = EFn.mkEffectFn1 \componentRef ->
  EFn.runEffectFn2 Ref.modify_ _ { rendering = true } componentRef

unlockRendering :: EFn.EffectFn1 ComponentRef Unit
unlockRendering = EFn.mkEffectFn1 \componentRef ->
  EFn.runEffectFn2 Ref.modify_ _ { rendering = false } componentRef

addComponentUnsubscriber :: EFn.EffectFn2 (Effect Unit) ComponentRef Unit
addComponentUnsubscriber = EFn.mkEffectFn2 \unsubscribe componentRef -> do
  { unsubscribers } <- EFn.runEffectFn1 Ref.read componentRef
  EFn.runEffectFn2 MA.snoc unsubscribers unsubscribe

addComponentHistory :: EFn.EffectFn2 VElement ComponentRef (Array VElement)
addComponentHistory = EFn.mkEffectFn2 \velement componentRef -> do
  { history } <- EFn.runEffectFn1 Ref.read componentRef
  EFn.runEffectFn2 MA.cons velement history
  EFn.runEffectFn2 MA.cutFrom 2 history
  pure $ MA.toArray history

componentRenderingLock :: EFn.EffectFn1 ComponentRef Boolean
componentRenderingLock = EFn.mkEffectFn1 \componentRef -> do
  { rendering } <- EFn.runEffectFn1 Ref.read componentRef
  pure rendering

componentHistory :: EFn.EffectFn1 ComponentRef (Array VElement)
componentHistory = EFn.mkEffectFn1 \componentRef -> do
  { history } <- EFn.runEffectFn1 Ref.read componentRef
  pure $ MA.toArray history

componentStore :: EFn.EffectFn1 ComponentRef Store
componentStore = EFn.mkEffectFn1 \componentRef -> do
  { store } <- EFn.runEffectFn1 Ref.read componentRef
  pure store

triggerUnsubscriber :: EFn.EffectFn1 ComponentRef Unit
triggerUnsubscriber = EFn.mkEffectFn1 \componentRef -> do
  { unsubscribers } <- EFn.runEffectFn1 Ref.read componentRef
  EFn.runEffectFn1 sequenceE $ MA.toArray unsubscribers
  EFn.runEffectFn1 MA.clear unsubscribers

getPortal :: Fn.Fn2 UIContext ComponentRef (Effect Node -> VNode -> VNode)
getPortal = Fn.mkFn2 \context componentRef -> \getPortalRoot vnode ->
  let createPortal = do
        parentNode <- getPortalRoot
        h <- EFn.runEffectFn2 addPortalHistory vnode componentRef
        EFn.runEffectFn2 registerParentNode parentNode context.nodeRefs
        EFn.runEffectFn2 diff (force patch)
          { context
          , parentNode
          , currents: []
          , nexts: [ vnode ]
          }

      updatePortal = do
        parentNode <- getPortalRoot
        h <- EFn.runEffectFn2 addPortalHistory vnode componentRef
        EFn.runEffectFn2 diff (force patch)
          { context
          , parentNode
          , currents: [ Fn.runFn2 byIdx h 1 ]
          , nexts: [ vnode ]
          }

      deletePortal = do
        parentNode <- getPortalRoot
        h <- EFn.runEffectFn1 componentPortalHistory componentRef
        EFn.runEffectFn2 diff (force patch)
          { context
          , parentNode
          , currents: [ Fn.runFn2 byIdx h 0 ]
          , nexts: []
          }
        EFn.runEffectFn2 unregisterParentNode parentNode context.nodeRefs

   in element "span"
        # didCreate (const createPortal)
        # didUpdate (const updatePortal)
        # didDelete (const deletePortal)

componentPortalHistory :: EFn.EffectFn1 ComponentRef (Array VNode)
componentPortalHistory = EFn.mkEffectFn1 \componentRef -> do
  { portalHistory } <- EFn.runEffectFn1 Ref.read componentRef
  pure $ MA.toArray portalHistory

addPortalHistory :: EFn.EffectFn2 VNode ComponentRef (Array VNode)
addPortalHistory = EFn.mkEffectFn2 \vnode componentRef -> do
  { portalHistory } <- EFn.runEffectFn1 Ref.read componentRef
  EFn.runEffectFn2 MA.cons vnode portalHistory
  EFn.runEffectFn2 MA.cutFrom 2 portalHistory
  pure $ MA.toArray portalHistory



type NodeRefs = MMap Node (MObject Node)

newNodeRefs :: Effect NodeRefs
newNodeRefs = MM.new

registerParentNode :: EFn.EffectFn2 Node NodeRefs Unit
registerParentNode = EFn.mkEffectFn2 \parent nodeRefs -> do
  mo <- MO.new
  EFn.runEffectFn3 MM.set parent mo nodeRefs

unregisterParentNode :: EFn.EffectFn2 Node NodeRefs Unit
unregisterParentNode = MM.del

registerChildNode :: EFn.EffectFn4 Node String Node NodeRefs Unit
registerChildNode = EFn.mkEffectFn4 \parent nodeKey child nodeRefs -> do
  mo <- EFn.runEffectFn2 MM.unsafeGet parent nodeRefs
  EFn.runEffectFn3 MO.set nodeKey child mo

unregisterChildNode :: EFn.EffectFn3 Node String NodeRefs Unit
unregisterChildNode = EFn.mkEffectFn3 \parent nodeKey nodeRefs -> do
  mo <- EFn.runEffectFn2 MM.unsafeGet parent nodeRefs
  EFn.runEffectFn2 MO.del nodeKey mo

getChildNode :: EFn.EffectFn3 Node String NodeRefs (Maybe Node)
getChildNode = EFn.mkEffectFn3 \parent nodeKey nodeRefs -> do
  mo <- EFn.runEffectFn2 MM.unsafeGet parent nodeRefs
  EFn.runEffectFn2 MO.get nodeKey mo
