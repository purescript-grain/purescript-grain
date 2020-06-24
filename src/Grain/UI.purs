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
import Data.Tuple (Tuple(..))
import Effect (Effect, forE)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify, modify_, new, read, write)
import Foreign.Object (Object, empty, insert)
import Grain.Class (class Grain, which)
import Grain.Effect (sequenceE, traverseE)
import Grain.Store (Store, createStore, readGrain, subscribeGrain, unsubscribeGrain, updateGrain)
import Grain.Styler (Styler, mountStyler)
import Grain.UI.Diff (class HasKey, diff)
import Grain.UI.Element (allocElement, updateElement)
import Grain.UI.Util (childNode, createText_, raf)
import Web.DOM.Element (Element)
import Web.DOM.Element as E
import Web.DOM.Node (Node, appendChild, insertBefore, removeChild, setTextContent)
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
  archive <- createArchive vnode
  patch
    { context: { store, styler, isSvg: false }
    , current: Nothing
    , next: Just archive
    , parentNode
    , nodeIndex: 0
    , moveIndex: Nothing
    }

type PatchArgs =
  { context :: UIContext
  , current :: Maybe Archive
  , next :: Maybe Archive
  , parentNode :: Node
  , nodeIndex :: Int
  , moveIndex :: Maybe Int
  }

type UIContext =
  { store :: Store
  , styler :: Styler
  , isSvg :: Boolean
  }

patch :: PatchArgs -> Effect Unit
patch { context, current, next, parentNode, nodeIndex, moveIndex } = do
  maybeNode <- patchElement current next $ switchContext current next context
  case maybeNode, current, next of
    Just node, Nothing, Just _ -> do
      maybeTarget <- childNode nodeIndex parentNode
      void case maybeTarget of
        Nothing -> appendChild node parentNode
        Just target -> insertBefore node target parentNode
    Just node, Just _, Nothing ->
      void $ removeChild node parentNode
    Just node, Just _, Just _ -> do
      case moveIndex of
        Nothing -> pure unit
        Just mi -> do
          let adjustedIdx = if nodeIndex < mi then mi + 1 else mi
          maybeAfterNode <- childNode adjustedIdx parentNode
          void case maybeAfterNode of
            Nothing -> appendChild node parentNode
            Just afterNode -> insertBefore node afterNode parentNode
    _, _, _ -> pure unit

switchContext
  :: Maybe Archive
  -> Maybe Archive
  -> UIContext
  -> UIContext
switchContext current next context =
  case current, next of
    _, Just (Tuple (VNode _ (VElement r)) _) ->
      if context.isSvg then context else context { isSvg = isSvg r }
    Just (Tuple (VNode _ (VElement r)) _), _ ->
      if context.isSvg then context else context { isSvg = isSvg r }
    _, _ -> context
  where
    isSvg r = r.tagName == "svg"

patchElement
  :: Maybe Archive
  -> Maybe Archive
  -> UIContext
  -> Effect (Maybe Node)
patchElement Nothing Nothing _ = pure Nothing
patchElement Nothing (Just (Tuple (VNode _ next) nextRef)) context =
  operateCreating (Tuple next nextRef) context
patchElement (Just (Tuple (VNode _ current) currentRef)) Nothing context =
  operateDeleting (Tuple current currentRef) context
patchElement (Just (Tuple (VNode _ current) currentRef)) (Just (Tuple (VNode _ next) nextRef)) context =
  if shouldUpdate current next
    then
      operateUpdating
        (Tuple current currentRef)
        (Tuple next nextRef)
        context
    else do
      maybeResult <- read currentRef
      write maybeResult nextRef
      case maybeResult of
        Nothing -> pure Nothing
        Just (View viewRef) ->
          Just <$> viewNode viewRef
        Just (Component componentRef) ->
          componentNode componentRef

shouldUpdate :: VElement -> VElement -> Boolean
shouldUpdate (VText current) (VText next) =
  current /= next
shouldUpdate (VElement current) (VElement next) =
  case current.fingerprint, next.fingerprint of
    Nothing, Nothing -> true
    cf, nf -> cf /= nf
shouldUpdate (VComponent current) (VComponent next) =
  case current.fingerprint, next.fingerprint of
    Nothing, Nothing -> true
    cf, nf -> cf /= nf
shouldUpdate _ _ = true

operateCreating
  :: Tuple VElement (Ref (Maybe Result))
  -> UIContext
  -> Effect (Maybe Node)
operateCreating (Tuple (VText str) resultRef) _ = do
  node <- createText_ str <#> T.toNode
  viewRef <- newViewRef node []
  write (Just $ View viewRef) resultRef
  pure $ Just node
operateCreating (Tuple (VElement r) resultRef) context = do
  el <- allocElement context.styler context.isSvg r
  nextChildren <- createArchives r.children
  let node = E.toNode el
  forE 0 (length nextChildren) \i ->
    patch
      { context
      , current: Nothing
      , next: nextChildren !! i
      , parentNode: node
      , nodeIndex: i
      , moveIndex: Nothing
      }
  viewRef <- newViewRef node nextChildren
  write (Just $ View viewRef) resultRef
  raf $ r.didCreate el
  pure $ Just node
operateCreating (Tuple (VComponent r) resultRef) context = do
  componentRef <- newComponentRef context r.render
  maybeNode <- evalComponent componentRef
  write (Just $ Component componentRef) resultRef
  pure maybeNode

operateDeleting
  :: Tuple VElement (Ref (Maybe Result))
  -> UIContext
  -> Effect (Maybe Node)
operateDeleting (Tuple (VText _) resultRef) _ = do
  maybeResult <- read resultRef
  case maybeResult of
    Just (View viewRef) ->
      Just <$> viewNode viewRef
    _ -> pure Nothing
operateDeleting (Tuple (VElement r) resultRef) context = do
  maybeResult <- read resultRef
  case maybeResult of
    Just (View viewRef) -> do
      node <- viewNode viewRef
      childrenHistory <- viewChildrenHistory viewRef
      let currentChildren = fromMaybe [] $ childrenHistory !! 0
      forE 0 (length currentChildren) \i ->
        patch
          { context
          , current: currentChildren !! i
          , next: Nothing
          , parentNode: node
          , nodeIndex: i
          , moveIndex: Nothing
          }
      case E.fromNode node of
        Nothing -> pure unit
        Just el -> raf $ r.didDelete el
      pure $ Just node
    _ -> pure Nothing
operateDeleting (Tuple (VComponent r) resultRef) _ = do
  maybeResult <- read resultRef
  case maybeResult of
    Just (Component componentRef) -> do
      unmountComponent componentRef
      componentNode componentRef
    _ -> pure Nothing

operateUpdating
  :: Tuple VElement (Ref (Maybe Result))
  -> Tuple VElement (Ref (Maybe Result))
  -> UIContext
  -> Effect (Maybe Node)
operateUpdating (Tuple (VText _) cRef) (Tuple (VText n) nRef) _ = do
  maybeResult <- read cRef
  case maybeResult of
    Just (View viewRef) -> do
      node <- viewNode viewRef
      setTextContent n node
      write (Just $ View viewRef) nRef
      pure $ Just node
    _ -> pure Nothing
operateUpdating (Tuple (VElement c) cRef) (Tuple (VElement n) nRef) context = do
  maybeResult <- read cRef
  case maybeResult of
    Just (View viewRef) -> do
      node <- viewNode viewRef
      case E.fromNode node of
        Just el -> do
          updateElement context.styler context.isSvg c n el
          childrenHistory <- addViewChildrenHistory n.children viewRef
          diff patch
            { context
            , parent: node
            , currentChildren: fromMaybe [] $ childrenHistory !! 1
            , nextChildren: fromMaybe [] $ childrenHistory !! 0
            }
          write (Just $ View viewRef) nRef
          raf $ n.didUpdate el
          pure $ Just node
        _ -> pure Nothing
    _ -> pure Nothing
operateUpdating (Tuple (VComponent c) cRef) (Tuple (VComponent n) nRef) _ = do
  maybeResult <- read cRef
  case maybeResult of
    Just (Component componentRef) -> do
      updateComponentRender n.render componentRef
      maybeNode <- evalComponent componentRef
      write (Just $ Component componentRef) nRef
      pure maybeNode
    _ -> pure Nothing
operateUpdating _ _ _ = pure Nothing



type ViewRef = Ref
  { node :: Node
  , childrenHistory :: Array (Array Archive)
  }

newViewRef :: Node -> Array Archive -> Effect ViewRef
newViewRef node children = new { node, childrenHistory: [ children ] }

addViewChildrenHistory :: Array VNode -> ViewRef -> Effect (Array (Array Archive))
addViewChildrenHistory vnodes viewRef = do
  archives <- createArchives vnodes
  _.childrenHistory <$> flip modify viewRef \r ->
    r { childrenHistory = archives : r.childrenHistory }

viewNode :: ViewRef -> Effect Node
viewNode viewRef =
  read viewRef <#> _.node

viewChildrenHistory :: ViewRef -> Effect (Array (Array Archive))
viewChildrenHistory viewRef =
  read viewRef <#> _.childrenHistory



type ComponentRef = Ref
  { node :: Maybe Node
  , rendering :: Boolean
  , unsubscribers :: Array (Effect Unit)
  , render :: Render VNode
  , history :: Array Archive
  , store :: Store
  , context :: UIContext
  , portalHistory :: Array Archive
  }

newComponentRef :: UIContext -> Render VNode -> Effect ComponentRef
newComponentRef context render = do
  store <- createStore
  new
    { node: Nothing
    , rendering: true
    , unsubscribers: []
    , render
    , history: []
    , store
    , context
    , portalHistory: []
    }

unmountComponent :: ComponentRef -> Effect Unit
unmountComponent componentRef = do
  triggerUnsubscriber componentRef
  history <- componentHistory componentRef
  context <- contextFromComponent componentRef
  node <- patchElement (history !! 0) Nothing context
  setComponentNode node componentRef

evalComponent :: ComponentRef -> Effect (Maybe Node)
evalComponent componentRef = do
  eval
  componentNode componentRef
  where
    storeSelection =
      { global: _, local: _ }
        <$> globalStoreFromComponent componentRef
        <*> localStoreFromComponent componentRef

    selectValue :: forall p a. Grain p a => p a -> Effect a
    selectValue proxy =
      which proxy <$> storeSelection >>= readGrain proxy

    listenValue :: forall p a. Grain p a => p a -> Effect Unit
    listenValue proxy = do
      store <- which proxy <$> storeSelection
      subscribeGrain proxy evalRaf store
      flip addComponentUnsubscriber componentRef
        $ unsubscribeGrain proxy evalRaf store

    updateValue :: forall p a. Grain p a => p a -> (a -> a) -> Effect Unit
    updateValue proxy f = do
      which proxy <$> storeSelection >>= updateGrain proxy f

    portalVNode = getPortal componentRef

    eval = do
      unlockRendering componentRef
      triggerUnsubscriber componentRef
      render <- componentRender componentRef
      vnode <- runRender render
        { selectValue
        , listenValue
        , updateValue
        , portalVNode
        }
      history <- addComponentHistory vnode componentRef
      context <- contextFromComponent componentRef
      node <- patchElement (history !! 1) (history !! 0) context
      setComponentNode node componentRef

    evalRaf = do
      locked <- componentRenderingLock componentRef
      when (not locked) do
        lockRendering componentRef
        raf eval

updateComponentRender :: Render VNode -> ComponentRef -> Effect Unit
updateComponentRender render = modify_ _ { render = render }

setComponentNode :: Maybe Node -> ComponentRef -> Effect Unit
setComponentNode node componentRef =
  flip modify_ componentRef _ { node = node }

lockRendering :: ComponentRef -> Effect Unit
lockRendering componentRef =
  flip modify_ componentRef _ { rendering = true }

unlockRendering :: ComponentRef -> Effect Unit
unlockRendering componentRef =
  flip modify_ componentRef _ { rendering = false }

addComponentUnsubscriber :: Effect Unit -> ComponentRef -> Effect Unit
addComponentUnsubscriber unsubscribe componentRef =
  flip modify_ componentRef \r ->
    r { unsubscribers = snoc r.unsubscribers unsubscribe }

addComponentHistory :: VNode -> ComponentRef -> Effect (Array Archive)
addComponentHistory vnode componentRef = do
  archive <- createArchive vnode
  _.history <$> flip modify componentRef \r ->
    r { history = take 2 $ archive : r.history }

componentNode :: ComponentRef -> Effect (Maybe Node)
componentNode componentRef =
  read componentRef <#> _.node

componentRender :: ComponentRef -> Effect (Render VNode)
componentRender componentRef =
  read componentRef <#> _.render

componentRenderingLock :: ComponentRef -> Effect Boolean
componentRenderingLock componentRef =
  read componentRef <#> _.rendering

componentHistory :: ComponentRef -> Effect (Array Archive)
componentHistory componentRef =
  read componentRef <#> _.history

contextFromComponent :: ComponentRef -> Effect UIContext
contextFromComponent componentRef =
  read componentRef <#> _.context

globalStoreFromComponent :: ComponentRef -> Effect Store
globalStoreFromComponent componentRef =
  contextFromComponent componentRef <#> _.store

localStoreFromComponent :: ComponentRef -> Effect Store
localStoreFromComponent componentRef =
  read componentRef <#> _.store

triggerUnsubscriber :: ComponentRef -> Effect Unit
triggerUnsubscriber componentRef = do
  { unsubscribers } <- read componentRef
  sequenceE unsubscribers
  flip modify_ componentRef _ { unsubscribers = [] }

getPortal :: ComponentRef -> Effect Node -> VNode -> VNode
getPortal componentRef getPortalRoot vnode =
  element "span"
    # didCreate (const patchPortal)
    # didUpdate (const patchPortal)
    # didDelete (const deletePortal)
  where
    patchPortal = do
      parentNode <- getPortalRoot
      context <- contextFromComponent componentRef
      history <- addPortalHistory vnode componentRef
      patch
        { context
        , current: history !! 1
        , next: history !! 0
        , parentNode
        , nodeIndex: 0
        , moveIndex: Nothing
        }

    deletePortal = do
      parentNode <- getPortalRoot
      context <- contextFromComponent componentRef
      history <- componentPortalHistory componentRef
      patch
        { context
        , current: history !! 0
        , next: Nothing
        , parentNode
        , nodeIndex: 0
        , moveIndex: Nothing
        }

componentPortalHistory :: ComponentRef -> Effect (Array Archive)
componentPortalHistory componentRef =
  read componentRef <#> _.portalHistory

addPortalHistory :: VNode -> ComponentRef -> Effect (Array Archive)
addPortalHistory vnode componentRef = do
  archive <- createArchive vnode
  _.portalHistory <$> flip modify componentRef \r ->
    r { portalHistory = take 2 $ archive : r.portalHistory }



type Archive = Tuple VNode (Ref (Maybe Result))

data Result
  = View ViewRef
  | Component ComponentRef

createArchive :: VNode -> Effect Archive
createArchive vnode = Tuple vnode <$> new Nothing

createArchives :: Array VNode -> Effect (Array Archive)
createArchives vnodes = traverseE createArchive vnodes
