module Grain.Internal.Diff
  ( class HasKey
  , getKey
  , PatchArgs(..)
  , diff
  ) where

import Prelude

import Data.Array (length)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect.Uncurried as EFn
import Grain.Internal.Effect (forE, foreachE, tailRecE)
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (byIdx, byIdxNullable, eqNullable, mapNullable, nonNull)

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
      args1 <- EFn.runEffectFn2 tailRecE diff1
        { done: false
        , patch
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
      void $ EFn.runEffectFn2 tailRecE diff2
        { done: false
        , patch
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
  { done :: Boolean
  , patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff1State
  }

diff1 :: forall ctx p c. HasKey c => EFn.EffectFn1 (Diff1Args ctx p c) (Diff1Args ctx p c)
diff1 = EFn.mkEffectFn1 \args1@{ patch, args, st } ->
  if st.startC > st.endC then
    pure args1 { done = true }
  else if st.startN > st.endN then
    pure args1 { done = true }
  else do
    let vnodeStartC = Fn.runFn2 byIdxNullable args.currents st.startC
        vnodeEndC = Fn.runFn2 byIdxNullable args.currents st.endC
        vnodeStartN = Fn.runFn2 byIdxNullable args.nexts st.startN
        vnodeEndN = Fn.runFn2 byIdxNullable args.nexts st.endN

        keyStartC = Fn.runFn2 mapNullable (getKey st.startC) vnodeStartC
        keyEndC = Fn.runFn2 mapNullable (getKey st.endC) vnodeEndC
        keyStartN = Fn.runFn2 mapNullable (getKey st.startN) vnodeStartN
        keyEndN = Fn.runFn2 mapNullable (getKey st.endN) vnodeEndN

        eqStart = Fn.runFn2 eqNullable keyStartC keyStartN
        eqEnd = Fn.runFn2 eqNullable keyEndC keyEndN
        eqStartEnd = Fn.runFn2 eqNullable keyStartC keyEndN
        eqEndStart = Fn.runFn2 eqNullable keyEndC keyStartN

    if eqStart then do
      EFn.runEffectFn1 patch $ Update
        { context: args.context
        , parentNode: args.parentNode
        , nodeKey: nonNull keyStartN
        , current: nonNull vnodeStartC
        , next: nonNull vnodeStartN
        }
      pure args1
        { st = st
            { startC = st.startC + 1
            , startN = st.startN + 1
            }
        }
    else if eqEnd then do
      EFn.runEffectFn1 patch $ Update
        { context: args.context
        , parentNode: args.parentNode
        , nodeKey: nonNull keyEndN
        , current: nonNull vnodeEndC
        , next: nonNull vnodeEndN
        }
      pure args1
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
        , nodeKey: nonNull keyEndN
        , index
        , current: nonNull vnodeStartC
        , next: nonNull vnodeEndN
        }
      pure args1
        { st = st
            { startC = st.startC + 1
            , endN = st.endN - 1
            }
        }
    else if eqEndStart then do
      EFn.runEffectFn1 patch $ Move
        { context: args.context
        , parentNode: args.parentNode
        , nodeKey: nonNull keyStartN
        , index: st.startN
        , current: nonNull vnodeEndC
        , next: nonNull vnodeStartN
        }
      pure args1
        { st = st
            { endC = st.endC - 1
            , startN = st.startN + 1
            }
        }
    else
      pure args1 { done = true }

type Diff2State =
  { startN :: Int
  , endN :: Int
  , ktoi :: MObject Int
  }

type Diff2Args ctx p c =
  { done :: Boolean
  , patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff2State
  }

diff2 :: forall ctx p c. HasKey c => EFn.EffectFn1 (Diff2Args ctx p c) (Diff2Args ctx p c)
diff2 = EFn.mkEffectFn1 \args2@{ patch, args, st } ->
  if st.startN <= st.endN then do
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
        pure args2
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
        pure args2
          { st = st { startN = st.startN + 1 }
          }
  else if MO.unsafeSize st.ktoi > 0 then do
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
    pure args2 { done = true }
  else
    pure args2 { done = true }

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
