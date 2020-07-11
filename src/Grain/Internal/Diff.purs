module Grain.Internal.Diff
  ( Patch
  , GetKey
  , Create
  , Delete
  , Update
  , Move
  , diff
  ) where

import Prelude

import Data.Array (length)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect.Uncurried as EFn
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (byIdx, byIdxNullable, eqNullable, forE, foreachE, keyNullable, nonNull, tailRecE)

type Patch ctx p c =
  { getKey :: GetKey c
  , create :: Create ctx p c
  , delete :: Delete ctx p c
  , update :: Update ctx p c
  , move :: Move ctx p c
  }

type GetKey c =
  Fn.Fn2 Int c String

type Create ctx p c =
  EFn.EffectFn5 ctx p String Int c Unit

type Delete ctx p c =
  EFn.EffectFn4 ctx p String c Unit

type Update ctx p c =
  EFn.EffectFn5 ctx p String c c Unit

type Move ctx p c =
  EFn.EffectFn6 ctx p String Int c c Unit

type DiffArgs ctx p c =
  { context :: ctx
  , parentNode :: p
  , currents :: Array c
  , nexts :: Array c
  }

diff
  :: forall ctx p c
   . EFn.EffectFn2 (Patch ctx p c) (DiffArgs ctx p c) Unit
diff = EFn.mkEffectFn2 \patch args -> do
  let lengthC = length args.currents
      lengthN = length args.nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let next = Fn.runFn2 byIdx args.nexts i
            nodeKey = Fn.runFn2 patch.getKey i next
        EFn.runEffectFn5 patch.create
          args.context
          args.parentNode
          nodeKey
          i
          next
    l, 0 ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let current = Fn.runFn2 byIdx args.currents i
            nodeKey = Fn.runFn2 patch.getKey i current
        EFn.runEffectFn4 patch.delete
          args.context
          args.parentNode
          nodeKey
          current
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
      EFn.runEffectFn1 diff2
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
  { done :: Boolean
  , patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff1State
  }

diff1 :: forall ctx p c. EFn.EffectFn1 (Diff1Args ctx p c) (Diff1Args ctx p c)
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

        keyStartC = Fn.runFn3 keyNullable patch.getKey st.startC vnodeStartC
        keyEndC = Fn.runFn3 keyNullable patch.getKey st.endC vnodeEndC
        keyStartN = Fn.runFn3 keyNullable patch.getKey st.startN vnodeStartN
        keyEndN = Fn.runFn3 keyNullable patch.getKey st.endN vnodeEndN

    if Fn.runFn2 eqNullable keyStartC keyStartN then do
      EFn.runEffectFn5 patch.update
        args.context
        args.parentNode
        (nonNull keyStartN)
        (nonNull vnodeStartC)
        (nonNull vnodeStartN)
      pure args1
        { st = st
            { startC = st.startC + 1
            , startN = st.startN + 1
            }
        }
    else if Fn.runFn2 eqNullable keyEndC keyEndN then do
      EFn.runEffectFn5 patch.update
        args.context
        args.parentNode
        (nonNull keyEndN)
        (nonNull vnodeEndC)
        (nonNull vnodeEndN)
      pure args1
        { st = st
            { endC = st.endC - 1
            , endN = st.endN - 1
            }
        }
    else if Fn.runFn2 eqNullable keyStartC keyEndN then do
      let delta = st.lengthN - 1 - st.endN
          index = st.lengthC - 1 - delta
      EFn.runEffectFn6 patch.move
        args.context
        args.parentNode
        (nonNull keyEndN)
        index
        (nonNull vnodeStartC)
        (nonNull vnodeEndN)
      pure args1
        { st = st
            { startC = st.startC + 1
            , endN = st.endN - 1
            }
        }
    else if Fn.runFn2 eqNullable keyEndC keyStartN then do
      EFn.runEffectFn6 patch.move
        args.context
        args.parentNode
        (nonNull keyStartN)
        st.startN
        (nonNull vnodeEndC)
        (nonNull vnodeStartN)
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
  { patch :: Patch ctx p c
  , args :: DiffArgs ctx p c
  , st :: Diff2State
  }

diff2 :: forall ctx p c. EFn.EffectFn1 (Diff2Args ctx p c) Unit
diff2 = EFn.mkEffectFn1 \{ patch, args, st } -> do
  EFn.runEffectFn3 forE st.startN (st.endN + 1)
    $ EFn.mkEffectFn1 \idx -> do
        let vnodeN = Fn.runFn2 byIdx args.nexts idx
            keyN = Fn.runFn2 patch.getKey idx vnodeN
        mIdxC <- EFn.runEffectFn2 MO.get keyN st.ktoi
        case mIdxC of
          Nothing ->
            EFn.runEffectFn5 patch.create
              args.context
              args.parentNode
              keyN
              idx
              vnodeN
          Just idxC -> do
            let vnodeC = Fn.runFn2 byIdx args.currents idxC
            EFn.runEffectFn6 patch.move
              args.context
              args.parentNode
              keyN
              idx
              vnodeC
              vnodeN
            EFn.runEffectFn2 MO.del keyN st.ktoi

  ks <- EFn.runEffectFn1 MO.keys st.ktoi
  EFn.runEffectFn2 foreachE ks $ EFn.mkEffectFn1 \keyC -> do
    idxC <- EFn.runEffectFn2 MO.unsafeGet keyC st.ktoi
    let vnodeC = Fn.runFn2 byIdx args.currents idxC
    EFn.runEffectFn4 patch.delete
      args.context
      args.parentNode
      keyC
      vnodeC

keyToIdx
  :: forall ctx p c
   . EFn.EffectFn1 (Diff1Args ctx p c) (MObject Int)
keyToIdx = EFn.mkEffectFn1 \{ patch, args, st } ->
  if st.startC > st.endC
    then MO.new
    else do
      ktoi <- MO.new
      EFn.runEffectFn3 forE st.startC (st.endC + 1) $ EFn.mkEffectFn1 \idx -> do
        let key = Fn.runFn2 patch.getKey idx $ Fn.runFn2 byIdx args.currents idx
        EFn.runEffectFn3 MO.set key idx ktoi
      pure ktoi
