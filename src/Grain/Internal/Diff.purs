module Grain.Internal.Diff
  ( class HasKey
  , getKey
  , PatchArgs(..)
  , diff
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, (!!))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried as EFn
import Grain.Internal.Effect (forE, foreachE)
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (byIdx, just)

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
  EFn.EffectFn1 (PatchArgs ctx p c) Unit

type DiffArgs ctx p c =
  { context :: ctx
  , parentNode :: p
  , currents :: Array c
  , nexts :: Array c
  }

diff
  :: forall ctx p c
   . HasKey c
  => EFn.EffectFn2 (Patch ctx p c) (DiffArgs ctx p c) Unit
diff = EFn.mkEffectFn2 \patch args@{ context, parentNode, currents, nexts } -> do
  let lengthC = length currents
      lengthN = length nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let next = Fn.runFn2 byIdx nexts i
            nodeKey = getKey i next
        EFn.runEffectFn1 patch $ Create
          { context
          , parentNode
          , nodeKey
          , index: i
          , next
          }
    l, 0 ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let current = Fn.runFn2 byIdx currents i
            nodeKey = getKey i current
        EFn.runEffectFn1 patch $ Delete
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
      ktoi <- EFn.runEffectFn1 keyToIdx args1
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

          eqStart = Fn.runFn2 eqKey keyStartC keyStartN
          eqEnd = Fn.runFn2 eqKey keyEndC keyEndN
          eqStartEnd = Fn.runFn2 eqKey keyStartC keyEndN
          eqEndStart = Fn.runFn2 eqKey keyEndC keyStartN

      if eqStart then do
        EFn.runEffectFn1 patch $ Update
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
        EFn.runEffectFn1 patch $ Update
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
        EFn.runEffectFn1 patch $ Move
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
        EFn.runEffectFn1 patch $ Move
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
      let vnodeN = Fn.runFn2 byIdx args.nexts st.startN
          keyN = getKey st.startN vnodeN
      mIdxC <- EFn.runEffectFn2 MO.get keyN st.ktoi
      case mIdxC of
        Nothing -> do
          EFn.runEffectFn1 patch $ Create
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
          let vnodeC = Fn.runFn2 byIdx args.currents idxC
          EFn.runEffectFn1 patch $ Move
            { context: args.context
            , parentNode: args.parentNode
            , nodeKey: keyN
            , index: st.startN
            , current: vnodeC
            , next: vnodeN
            }
          EFn.runEffectFn2 MO.del keyN st.ktoi
          pure $ Loop args2
            { st = st { startN = st.startN + 1 }
            }
  | MO.unsafeSize st.ktoi > 0 = do
      ks <- EFn.runEffectFn1 MO.keys st.ktoi
      EFn.runEffectFn2 foreachE ks $ EFn.mkEffectFn1 \keyC -> do
        idxC <- EFn.runEffectFn2 MO.unsafeGet keyC st.ktoi
        let vnodeC = Fn.runFn2 byIdx args.currents idxC
        EFn.runEffectFn1 patch $ Delete
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
  => EFn.EffectFn1 (Diff1Args ctx p c) (MObject Int)
keyToIdx = EFn.mkEffectFn1 \{ args, st } ->
  if st.startC > st.endC
    then MO.new
    else do
      ktoi <- MO.new
      EFn.runEffectFn3 forE st.startC (st.endC + 1) $ EFn.mkEffectFn1 \idx -> do
        let key = getKey idx $ Fn.runFn2 byIdx args.currents idx
        EFn.runEffectFn3 MO.set key idx ktoi
      pure ktoi

eqKey :: Fn.Fn2 (Maybe String) (Maybe String) Boolean
eqKey = Fn.mkFn2 \mc mn ->
  case mc, mn of
    Just c, Just n ->
      c == n
    _, _ ->
      false
