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

import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (length, snoc, take, (!!), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object, empty, insert)
import Grain.Class (class Grain, which)
import Grain.Effect (forE, sequenceE)
import Grain.JSMap (JSMap)
import Grain.JSMap as JM
import Grain.Store (Store, createStore, readGrain, subscribeGrain, unsubscribeGrain, updateGrain)
import Grain.Styler (Styler, mountStyler)
import Grain.UI.Diff (class HasKey, diff)
import Grain.UI.Element (allocElement, updateElement)
import Grain.UI.Util (childNode, createText_, putNode, raf)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Web.DOM.Element as E
import Web.DOM.Node (Node, removeChild, setTextContent)
import Web.DOM.Text as T
import Web.Event.Event (Event)



-- | The type of virtual node.
data VNode = VNode (Maybe String) VElement

data VElement
  = VText String
  | VElement
      { fingerprint :: Maybe String
      , tagName :: String
      , props :: Object String
      , handlers :: Object (Event -> Effect Unit)
      , children :: Array VNode
      , didCreate :: Element -> Effect Unit
      , didUpdate :: Element -> Effect Unit
      , didDelete :: Element -> Effect Unit
      }
  | VComponent
      { fingerprint :: Maybe String
      , render :: Render VNode
      }

instance hasKeyVNode :: HasKey VNode where
  getKey idx (VNode k velement) =
    case velement of
      VText _ ->
        "text_" <> identifier
      VElement { tagName } ->
        "element_" <> tagName <> "_" <> identifier
      VComponent _ ->
        "component_" <> identifier
    where
      identifier = fromMaybe (show idx) k

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
  , props: empty
  , handlers: empty
  , children: []
  , didCreate: const $ pure unit
  , didUpdate: const $ pure unit
  , didDelete: const $ pure unit
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
  VNode k $ VElement r { props = insert name val r.props }
prop _ _ velement = velement

-- | Bind an event handler.
handle
  :: String
  -> (Event -> Effect Unit)
  -> VNode
  -> VNode
handle name handler (VNode k (VElement r)) =
  VNode k $ VElement r { handlers = insert name handler r.handlers }
handle _ _ velement = velement

-- | Bind `didCreate` lifecycle.
didCreate
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didCreate handler (VNode k (VElement r)) =
  VNode k $ VElement r { didCreate = handler }
didCreate _ velement = velement

-- | Bind `didUpdate` lifecycle.
didUpdate
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didUpdate handler (VNode k (VElement r)) =
  VNode k $ VElement r { didUpdate = handler }
didUpdate _ velement = velement

-- | Bind `didDelete` lifecycle.
didDelete
  :: (Element -> Effect Unit)
  -> VNode
  -> VNode
didDelete handler (VNode k (VElement r)) =
  VNode k $ VElement r { didDelete = handler }
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
  withReaderT (const query) $ liftEffect do
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
  componentRefs <- JM.new
  patch
    { context:
        { isSvg: false
        , deleting: false
        , store
        , styler
        , componentRefs
        }
    , parentNode
    , nodeIndex: 0
    , moveIndex: Nothing
    , current: Nothing
    , next: Just vnode
    }



type UIContext =
  { isSvg :: Boolean
  , deleting :: Boolean
  , store :: Store
  , styler :: Styler
  , componentRefs :: JSMap Node ComponentRef
  }

switchToSvg :: String -> UIContext -> UIContext
switchToSvg tag context =
  if context.isSvg
    then context
    else context { isSvg = tag == "svg" }

switchToDeleting :: UIContext -> UIContext
switchToDeleting context =
  if context.deleting
    then context
    else context { deleting = true }



type PatchArgs =
  { context :: UIContext
  , parentNode :: Node
  , nodeIndex :: Int
  , moveIndex :: Maybe Int
  , current :: Maybe VNode
  , next :: Maybe VNode
  }

patch :: PatchArgs -> Effect Unit
patch { context, parentNode, nodeIndex, moveIndex, current, next } =
  case current, next of
    Nothing, Just (VNode _ next') -> do
      node <- eval { context, target: Nothing, current: Nothing, next: Just next' }
      void $ putNode nodeIndex node parentNode
    Just (VNode _ current'), Nothing -> do
      let ctx = switchToDeleting context
      target <- childNode nodeIndex parentNode
      node <- eval { context: ctx, target, current: Just current', next: Nothing }
      when (not context.deleting) do
        void $ removeChild node parentNode
    Just (VNode _ current'), Just (VNode _ next') -> do
      target <- childNode nodeIndex parentNode
      node <- eval { context, target, current: Just current', next: Just next' }
      case moveIndex of
        Nothing -> pure unit
        Just mi -> do
          let adjustedIdx = if nodeIndex < mi then mi + 1 else mi
          void $ putNode adjustedIdx node parentNode
    _, _ ->
      pure unit



type EvalArgs =
  { context :: UIContext
  , target :: Maybe Node
  , current :: Maybe VElement
  , next :: Maybe VElement
  }

eval :: EvalArgs -> Effect Node
eval { context, target, current, next } =
  case target, current, next of
    -- Create
    Nothing, Nothing, Just (VText nt) ->
      createText_ nt <#> T.toNode
    Nothing, Nothing, Just (VComponent nc) -> do
      componentRef <- newComponentRef
      node <- evalComponent
        { context
        , target: Nothing
        , render: nc.render
        , componentRef
        }
      JM.set node componentRef context.componentRefs
      pure node
    Nothing, Nothing, Just (VElement nv) -> do
      let ctx = switchToSvg nv.tagName context
      el <- allocElement
        { isSvg: ctx.isSvg
        , styler: ctx.styler
        , next: nv
        }
      let node = E.toNode el
      forE 0 (length nv.children) \i ->
        patch
          { context: ctx
          , parentNode: node
          , nodeIndex: i
          , moveIndex: Nothing
          , current: Nothing
          , next: nv.children !! i
          }
      nv.didCreate el
      pure node

    -- Delete
    Just node, Just (VText _), Nothing ->
      pure node
    Just node, Just (VComponent _), Nothing -> do
      componentRef <- JM.unsafeGet node context.componentRefs
      unmountComponent
        { context
        , target: Just node
        , componentRef
        }
      JM.del node context.componentRefs
      pure node
    Just node, Just (VElement cv), Nothing -> do
      forE 0 (length cv.children) \i ->
        patch
          { context
          , parentNode: node
          , nodeIndex: i
          , moveIndex: Nothing
          , current: cv.children !! i
          , next: Nothing
          }
      cv.didDelete $ unsafeCoerce node
      pure node

    -- Update
    Just node, Just (VText ct), Just (VText nt) -> do
      when (ct /= nt) do
        setTextContent nt node
      pure node
    Just node, Just (VComponent cc), Just (VComponent nc) -> do
      when (isDifferent cc nc) do
        componentRef <- JM.unsafeGet node context.componentRefs
        void $ evalComponent
          { context
          , target: Just node
          , render: nc.render
          , componentRef
          }
      pure node
    Just node, Just (VElement cv), Just (VElement nv) -> do
      when (isDifferent cv nv) do
        let el = unsafeCoerce node
            ctx = switchToSvg nv.tagName context
        updateElement
          { isSvg: ctx.isSvg
          , styler: ctx.styler
          , current: cv
          , next: nv
          , element: el
          }
        diff patch
          { context: ctx
          , parent: node
          , currentChildren: cv.children
          , nextChildren: nv.children
          }
        nv.didUpdate el
      pure node

    _, _, _ ->
      throw "Renderer can't evaluate vnodes."

isDifferent
  :: forall r
   . { fingerprint :: Maybe String | r }
  -> { fingerprint :: Maybe String | r }
  -> Boolean
isDifferent { fingerprint: Nothing } { fingerprint: Nothing } =
  true
isDifferent { fingerprint: cf } { fingerprint: nf } =
  cf /= nf



type ComponentRef = Ref
  { rendering :: Boolean
  , unsubscribers :: Array (Effect Unit)
  , history :: Array VElement
  , store :: Store
  , portalHistory :: Array VNode
  }

newComponentRef :: Effect ComponentRef
newComponentRef = do
  store <- createStore
  Ref.new
    { rendering: true
    , unsubscribers: []
    , history: []
    , store
    , portalHistory: []
    }

type EvalComponentArgs =
  { context :: UIContext
  , target :: Maybe Node
  , render :: Render VNode
  , componentRef :: ComponentRef
  }

evalComponent :: EvalComponentArgs -> Effect Node
evalComponent { context, target, render, componentRef } = do
  targetRef <- Ref.new target

  let storeSelection =
        componentStore componentRef
          <#> { global: context.store, local: _ }

      listenValue :: forall p a. Grain p a => p a -> Effect Unit
      listenValue proxy = do
        store <- which proxy <$> storeSelection
        subscribeGrain proxy evaluateRaf store
        flip addComponentUnsubscriber componentRef
          $ unsubscribeGrain proxy evaluateRaf store

      selectValue :: forall p a. Grain p a => p a -> Effect a
      selectValue proxy =
        which proxy <$> storeSelection >>= readGrain proxy

      updateValue :: forall p a. Grain p a => p a -> (a -> a) -> Effect Unit
      updateValue proxy f = do
        which proxy <$> storeSelection >>= updateGrain proxy f

      portalVNode = getPortal context componentRef

      evaluate = do
        unlockRendering componentRef
        triggerUnsubscriber componentRef
        VNode _ velement <- runRender render
          { selectValue
          , listenValue
          , updateValue
          , portalVNode
          }
        h <- addComponentHistory velement componentRef
        t <- Ref.read targetRef
        eval { context, target: t, current: h !! 1, next: h !! 0 }

      evaluateRaf = do
        locked <- componentRenderingLock componentRef
        when (not locked) do
          lockRendering componentRef
          raf $ void evaluate

  node <- evaluate
  Ref.write (Just node) targetRef
  pure node

type UnmountComponentArgs =
  { context :: UIContext
  , target :: Maybe Node
  , componentRef :: ComponentRef
  }

unmountComponent :: UnmountComponentArgs -> Effect Unit
unmountComponent { context, target, componentRef } = do
  triggerUnsubscriber componentRef
  h <- componentHistory componentRef
  void $ eval { context, target, current: h !! 0, next: Nothing }

lockRendering :: ComponentRef -> Effect Unit
lockRendering componentRef =
  flip Ref.modify_ componentRef _ { rendering = true }

unlockRendering :: ComponentRef -> Effect Unit
unlockRendering componentRef =
  flip Ref.modify_ componentRef _ { rendering = false }

addComponentUnsubscriber :: Effect Unit -> ComponentRef -> Effect Unit
addComponentUnsubscriber unsubscribe componentRef =
  flip Ref.modify_ componentRef \r ->
    r { unsubscribers = snoc r.unsubscribers unsubscribe }

addComponentHistory :: VElement -> ComponentRef -> Effect (Array VElement)
addComponentHistory velement componentRef =
  _.history <$> flip Ref.modify componentRef \r ->
    r { history = take 2 $ velement : r.history }

componentRenderingLock :: ComponentRef -> Effect Boolean
componentRenderingLock componentRef =
  Ref.read componentRef <#> _.rendering

componentHistory :: ComponentRef -> Effect (Array VElement)
componentHistory componentRef =
  Ref.read componentRef <#> _.history

componentStore :: ComponentRef -> Effect Store
componentStore componentRef =
  Ref.read componentRef <#> _.store

triggerUnsubscriber :: ComponentRef -> Effect Unit
triggerUnsubscriber componentRef = do
  { unsubscribers } <- Ref.read componentRef
  sequenceE unsubscribers
  flip Ref.modify_ componentRef _ { unsubscribers = [] }

getPortal :: UIContext -> ComponentRef -> Effect Node -> VNode -> VNode
getPortal context componentRef getPortalRoot vnode =
  element "span"
    # didCreate (const patchPortal)
    # didUpdate (const patchPortal)
    # didDelete (const deletePortal)
  where
    patchPortal = do
      parentNode <- getPortalRoot
      h <- addPortalHistory vnode componentRef
      patch
        { context
        , parentNode
        , nodeIndex: 0
        , moveIndex: Nothing
        , current: h !! 1
        , next: h !! 0
        }

    deletePortal = do
      parentNode <- getPortalRoot
      h <- componentPortalHistory componentRef
      patch
        { context
        , parentNode
        , nodeIndex: 0
        , moveIndex: Nothing
        , current: h !! 0
        , next: Nothing
        }

componentPortalHistory :: ComponentRef -> Effect (Array VNode)
componentPortalHistory componentRef =
  Ref.read componentRef <#> _.portalHistory

addPortalHistory :: VNode -> ComponentRef -> Effect (Array VNode)
addPortalHistory vnode componentRef =
  _.portalHistory <$> flip Ref.modify componentRef \r ->
    r { portalHistory = take 2 $ vnode : r.portalHistory }
