module Grain.UI.Diff
  ( class HasKey
  , getKey
  , diff
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Array (length, snoc, unsafeIndex, (!!), (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)

class HasKey a where
  getKey :: Int -> a -> String

instance hasKeyInt :: HasKey Int where
  getKey _ = show

instance hasKeyTuple :: (HasKey a) => HasKey (Tuple a b) where
  getKey idx (Tuple x _) = getKey idx x

type PatchArgs parent child =
  { current :: Maybe child
  , next :: Maybe child
  , parentNode :: parent
  , nodeIndex :: Int
  , moveIndex :: Maybe Int
  }

type Patcher parent child m =
  PatchArgs parent child -> m Unit

data RealOperation = Add | Delete

type DiffState a =
  { currents :: Array a
  , nexts :: Array a
  , currentStart :: Int
  , currentEnd :: Int
  , nextStart :: Int
  , nextEnd :: Int
  , realOperations :: Array (Tuple Int RealOperation)
  , keyToIndex :: O.Object Int
  }

initState
  :: forall a
   . HasKey a
  => Array a
  -> Array a
  -> DiffState a
initState currents nexts =
  { currents
  , nexts
  , currentStart: 0
  , currentEnd: length currents - 1
  , nextStart: 0
  , nextEnd: length nexts - 1
  , realOperations: []
  , keyToIndex: O.empty
  }

cursorCurrentStart :: forall a. DiffState a -> Maybe a
cursorCurrentStart { currents, currentStart } = currents !! currentStart

cursorCurrentEnd :: forall a. DiffState a -> Maybe a
cursorCurrentEnd { currents, currentEnd } = currents !! currentEnd

cursorNextStart :: forall a. DiffState a -> Maybe a
cursorNextStart { nexts, nextStart } = nexts !! nextStart

cursorNextEnd :: forall a. DiffState a -> Maybe a
cursorNextEnd { nexts, nextEnd } = nexts !! nextEnd

forwardCurrentCursor :: forall a. DiffState a -> DiffState a
forwardCurrentCursor x = x { currentStart = x.currentStart + 1 }

forwardNextCursor :: forall a. DiffState a -> DiffState a
forwardNextCursor x = x { nextStart = x.nextStart + 1 }

backwardCurrentCursor :: forall a. DiffState a -> DiffState a
backwardCurrentCursor x = x { currentEnd = x.currentEnd - 1 }

backwardNextCursor :: forall a. DiffState a -> DiffState a
backwardNextCursor x = x { nextEnd = x.nextEnd - 1 }

markRealAdding :: forall a. Int -> DiffState a -> DiffState a
markRealAdding realIndex x = x { realOperations = snoc x.realOperations $ Tuple realIndex Add }

markRealDeleting :: forall a. Int -> DiffState a -> DiffState a
markRealDeleting realIndex x = x { realOperations = snoc x.realOperations $ Tuple realIndex Delete }

markRealMove :: forall a. Int -> Int -> DiffState a -> DiffState a
markRealMove from to =
  markRealDeleting from >>> markRealAdding to

realSourceIdx :: forall a. Int -> DiffState a -> Int
realSourceIdx currentVirtualIdx x =
  foldl calc currentVirtualIdx x.realOperations
  where
    calc sourceIdx (Tuple idx Add) = if sourceIdx < idx then sourceIdx else sourceIdx + 1
    calc sourceIdx (Tuple idx Delete) = if sourceIdx < idx then sourceIdx else sourceIdx - 1

storeKeyToIdx :: forall a. HasKey a => DiffState a -> DiffState a
storeKeyToIdx x = x { keyToIndex = _ }
  if x.currentStart > x.currentEnd
    then O.empty
    else O.fromFoldable
      $ flip map (x.currentStart .. x.currentEnd)
      $ \i -> Tuple (getKey i $ unsafePartial $ unsafeIndex x.currents i) i

markProcessKey :: forall a. String -> DiffState a -> DiffState a
markProcessKey k x = x { keyToIndex = _ } $ O.delete k x.keyToIndex

diff
  :: forall parent child m
   . HasKey child
  => MonadRec m
  => Patcher parent child m
  -> parent
  -> Array child
  -> Array child
  -> m Unit
diff patch parent currentChildren nextChildren = do
  state <- storeKeyToIdx <$> tailRecM takeDiffPerformant initialState
  tailRecM takeDiff state
  where
    initialState = initState currentChildren nextChildren

    equalKey (Just c) (Just n) = c == n
    equalKey _ _ = false

    takeDiffPerformant s
      | s.currentStart > s.currentEnd || s.nextStart > s.nextEnd = pure $ Done s
      | equalKey (getKey s.currentStart <$> cursorCurrentStart s) (getKey s.nextStart <$> cursorNextStart s) = do
          patch
            { current: cursorCurrentStart s
            , next: cursorNextStart s
            , parentNode: parent
            , nodeIndex: s.currentStart
            , moveIndex: Nothing
            }
          pure $ Loop $ forwardCurrentCursor >>> forwardNextCursor $ s
      | equalKey (getKey s.currentEnd <$> cursorCurrentEnd s) (getKey s.nextEnd <$> cursorNextEnd s) = do
          patch
            { current: cursorCurrentEnd s
            , next: cursorNextEnd s
            , parentNode: parent
            , nodeIndex: s.currentEnd
            , moveIndex: Nothing
            }
          pure $ Loop $ backwardCurrentCursor >>> backwardNextCursor $ s
      | otherwise = pure $ Done s

    takeDiff s
      | s.nextStart <= s.nextEnd =
          let cursorNext = unsafePartial $ unsafeIndex s.nexts s.nextStart
              nextKey = getKey s.nextStart cursorNext
           in case O.lookup nextKey s.keyToIndex of
                Nothing -> do
                  patch
                    { current: Nothing
                    , next: Just cursorNext
                    , parentNode: parent
                    , nodeIndex: s.nextStart
                    , moveIndex: Nothing
                    }
                  pure $ Loop $ markRealAdding s.nextStart
                    >>> forwardNextCursor $ s
                Just currentIdx -> do
                  let sourceIdx = realSourceIdx currentIdx s
                      targetIdx = s.nextStart
                  patch
                    { current: s.currents !! currentIdx
                    , next: Just cursorNext
                    , parentNode: parent
                    , nodeIndex: sourceIdx
                    , moveIndex: Just targetIdx
                    }
                  pure $ Loop $ markRealMove sourceIdx targetIdx
                    >>> markProcessKey nextKey
                    >>> forwardNextCursor $ s
      | O.size s.keyToIndex > 0 = do
          let currentKey = unsafePartial $ unsafeIndex (O.keys s.keyToIndex) 0
              currentIdx = unsafePartial $ fromJust $ O.lookup currentKey s.keyToIndex
              sourceIdx = realSourceIdx currentIdx s
          patch
            { current: s.currents !! currentIdx
            , next: Nothing
            , parentNode: parent
            , nodeIndex: sourceIdx
            , moveIndex: Nothing
            }
          pure $ Loop $ markRealDeleting sourceIdx
            >>> markProcessKey currentKey $ s
      | otherwise = pure $ Done unit
