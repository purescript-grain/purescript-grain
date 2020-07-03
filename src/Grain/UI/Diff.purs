module Grain.UI.Diff
  ( class HasKey
  , getKey
  , PatchArgs(..)
  , diff
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Grain.Effect (forE, foreachE)
import Grain.MObject (MObject)
import Grain.MObject as MO
import Grain.UI.Util (index_, just)

class HasKey c where
  getKey :: Int -> c -> String

instance hasKeyInt :: HasKey Int where
  getKey _ = show

data PatchArgs ctx p c
  = Create
      { context :: ctx
      , parentNode :: p
      , nodeKey :: String
      , index :: Int
      , next :: c
      }
  | Delete
      { context :: ctx
      , parentNode :: p
      , nodeKey :: String
      , current :: c
      }
  | Update
      { context :: ctx
      , parentNode :: p
      , nodeKey :: String
      , current :: c
      , next :: c
      }
  | Move
      { context :: ctx
      , parentNode :: p
      , nodeKey :: String
      , index :: Int
      , current :: c
      , next :: c
      }

type Patch ctx p c =
  PatchArgs ctx p c -> Effect Unit

type DiffArgs ctx p c =
  { context :: ctx
  , parentNode :: p
  , currents :: Array c
  , nexts :: Array c
  }

diff
  :: forall ctx p c
   . HasKey c
  => Patch ctx p c
  -> DiffArgs ctx p c
  -> Effect Unit
diff patch args@{ context, parentNode, currents, nexts } = do
  let lengthC = length currents
      lengthN = length nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      forE 0 l \i -> do
        let next = index_ nexts i
            nodeKey = getKey i next
        patch $ Create
          { context
          , parentNode
          , nodeKey
          , index: i
          , next
          }
    l, 0 ->
      forE 0 l \i -> do
        let current = index_ currents i
            nodeKey = getKey i current
        patch $ Delete
          { context
          , parentNode
          , nodeKey
          , current
          }
    _, _ -> do
      args1 <- tailRecM diff1
        { patch
        , args
        , st:
            { lengthC
            , startC: 0
            , endC: lengthC - 1
            , lengthN
            , startN: 0
            , endN: lengthN - 1
            }
        }
      ktoi <- keyToIdx args1
      tailRecM diff2
        { patch
        , args
        , st:
            { startN: args1.st.startN
            , endN: args1.st.endN
            , ktoi
            }
        }

type Diff1State =
  { lengthC :: Int
  , startC :: Int
  , endC :: Int
  , lengthN :: Int
  , startN :: Int
  , endN :: Int
  }

type Diff1Args ctx p c =
  { patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff1State
  }

diff1
  :: forall ctx p c
   . HasKey c
  => Diff1Args ctx p c
  -> Effect (Step (Diff1Args ctx p c) (Diff1Args ctx p c))
diff1 args1@{ patch, args, st }
  | st.startC > st.endC =
      pure $ Done args1
  | st.startN > st.endN =
      pure $ Done args1
  | otherwise = do
      let vnodeStartC = args.currents !! st.startC
          vnodeEndC = args.currents !! st.endC
          vnodeStartN = args.nexts !! st.startN
          vnodeEndN = args.nexts !! st.endN

          keyStartC = getKey st.startC <$> vnodeStartC
          keyEndC = getKey st.endC <$> vnodeEndC
          keyStartN = getKey st.startN <$> vnodeStartN
          keyEndN = getKey st.endN <$> vnodeEndN

          eqStart = eqKey keyStartC keyStartN
          eqEnd = eqKey keyEndC keyEndN
          eqStartEnd = eqKey keyStartC keyEndN
          eqEndStart = eqKey keyEndC keyStartN

      if eqStart then do
        patch $ Update
          { context: args.context
          , parentNode: args.parentNode
          , nodeKey: just keyStartN
          , current: just vnodeStartC
          , next: just vnodeStartN
          }
        pure $ Loop args1
          { st = st
              { startC = st.startC + 1
              , startN = st.startN + 1
              }
          }
      else if eqEnd then do
        patch $ Update
          { context: args.context
          , parentNode: args.parentNode
          , nodeKey: just keyEndN
          , current: just vnodeEndC
          , next: just vnodeEndN
          }
        pure $ Loop args1
          { st = st
              { endC = st.endC - 1
              , endN = st.endN - 1
              }
          }
      else if eqStartEnd then do
        let delta = st.lengthN - 1 - st.endN
            index = st.lengthC - 1 - delta
        patch $ Move
          { context: args.context
          , parentNode: args.parentNode
          , nodeKey: just keyEndN
          , index
          , current: just vnodeStartC
          , next: just vnodeEndN
          }
        pure $ Loop args1
          { st = st
              { startC = st.startC + 1
              , endN = st.endN - 1
              }
          }
      else if eqEndStart then do
        patch $ Move
          { context: args.context
          , parentNode: args.parentNode
          , nodeKey: just keyStartN
          , index: st.startN
          , current: just vnodeEndC
          , next: just vnodeStartN
          }
        pure $ Loop args1
          { st = st
              { endC = st.endC - 1
              , startN = st.startN + 1
              }
          }
      else
        pure $ Done args1

type Diff2State =
  { startN :: Int
  , endN :: Int
  , ktoi :: MObject Int
  }

type Diff2Args ctx p c =
  { patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff2State
  }

diff2
  :: forall ctx p c
   . HasKey c
  => Diff2Args ctx p c
  -> Effect (Step (Diff2Args ctx p c) Unit)
diff2 args2@{ patch, args, st }
  | st.startN <= st.endN = do
      let vnodeN = index_ args.nexts st.startN
          keyN = getKey st.startN vnodeN
      mIdxC <- MO.get keyN st.ktoi
      case mIdxC of
        Nothing -> do
          patch $ Create
            { context: args.context
            , parentNode: args.parentNode
            , nodeKey: keyN
            , index: st.startN
            , next: vnodeN
            }
          pure $ Loop args2
            { st = st
                { startN = st.startN + 1
                }
            }
        Just idxC -> do
          let vnodeC = index_ args.currents idxC
          patch $ Move
            { context: args.context
            , parentNode: args.parentNode
            , nodeKey: keyN
            , index: st.startN
            , current: vnodeC
            , next: vnodeN
            }
          MO.del keyN st.ktoi
          pure $ Loop args2
            { st = st { startN = st.startN + 1 }
            }
  | unsafePerformEffect (MO.size st.ktoi) > 0 = do
      ks <- MO.keys st.ktoi
      foreachE ks \keyC -> do
        idxC <- MO.unsafeGet keyC st.ktoi
        let vnodeC = index_ args.currents idxC
        patch $ Delete
          { context: args.context
          , parentNode: args.parentNode
          , nodeKey: keyC
          , current: vnodeC
          }
      pure $ Done unit
  | otherwise =
      pure $ Done unit

keyToIdx
  :: forall ctx p c
   . HasKey c
  => Diff1Args ctx p c
  -> Effect (MObject Int)
keyToIdx { args: { currents }, st: { startC, endC } }
  | startC > endC = MO.new
  | otherwise = do
      ktoi <- MO.new
      forE startC (endC + 1) \idx ->
        MO.set (getKey idx $ index_ currents idx) idx ktoi
      pure ktoi

eqKey :: Maybe String -> Maybe String -> Boolean
eqKey (Just c) (Just n) = c == n
eqKey _ _ = false
