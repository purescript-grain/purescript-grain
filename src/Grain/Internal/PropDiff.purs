module Grain.Internal.PropDiff
  ( Patch
  , Create
  , Delete
  , Update
  , diff
  ) where

import Prelude

import Data.Array (length)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Effect.Uncurried as EFn
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (byIdx, byIdxNullable, eqNullable, forE, foreachE, mapNullable, nonNull, tailRecE)

type Patch ctx a =
  { create :: Create ctx a
  , delete :: Delete ctx a
  , update :: Update ctx a
  }

type Create ctx a =
  EFn.EffectFn2 ctx (Tuple String a) Unit

type Delete ctx a =
  EFn.EffectFn2 ctx (Tuple String a) Unit

type Update ctx a =
  EFn.EffectFn3 ctx (Tuple String a) (Tuple String a) Unit

type DiffArgs ctx a =
  { context :: ctx
  , currents :: Array (Tuple String a)
  , nexts :: Array (Tuple String a)
  }

diff
  :: forall ctx a
   . EFn.EffectFn2 (Patch ctx a) (DiffArgs ctx a) Unit
diff = EFn.mkEffectFn2 \patch args -> do
  let lengthC = length args.currents
      lengthN = length args.nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let next = Fn.runFn2 byIdx args.nexts i
        EFn.runEffectFn2 patch.create args.context next
    l, 0 ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let current = Fn.runFn2 byIdx args.currents i
        EFn.runEffectFn2 patch.delete args.context current
    _, _ -> do
      args1 <- EFn.runEffectFn2 tailRecE diff1
        { done: false
        , patch
        , args
        , st:
            { startC: 0
            , endC: lengthC - 1
            , startN: 0
            , endN: lengthN - 1
            }
        }
      ntoi <- EFn.runEffectFn1 nameToIdx args1
      EFn.runEffectFn1 diff2
        { patch
        , args
        , st:
            { startN: args1.st.startN
            , endN: args1.st.endN
            , ntoi
            }
        }

type Diff1State =
  { startC :: Int
  , endC :: Int
  , startN :: Int
  , endN :: Int
  }

type Diff1Args ctx a =
  { done :: Boolean
  , patch :: Patch ctx a
  , args :: DiffArgs ctx a
  , st :: Diff1State
  }

diff1 :: forall ctx a. EFn.EffectFn1 (Diff1Args ctx a) (Diff1Args ctx a)
diff1 = EFn.mkEffectFn1 \args1@{ patch, args, st } ->
  if st.startC > st.endC then
    pure args1 { done = true }
  else if st.startN > st.endN then
    pure args1 { done = true }
  else do
    let tupleStartC = Fn.runFn2 byIdxNullable args.currents st.startC
        tupleEndC = Fn.runFn2 byIdxNullable args.currents st.endC
        tupleStartN = Fn.runFn2 byIdxNullable args.nexts st.startN
        tupleEndN = Fn.runFn2 byIdxNullable args.nexts st.endN

        nameStartC = Fn.runFn2 mapNullable fst tupleStartC
        nameEndC = Fn.runFn2 mapNullable fst tupleEndC
        nameStartN = Fn.runFn2 mapNullable fst tupleStartN
        nameEndN = Fn.runFn2 mapNullable fst tupleEndN

    if Fn.runFn2 eqNullable nameStartC nameStartN then do
      EFn.runEffectFn3 patch.update
        args.context
        (nonNull tupleStartC)
        (nonNull tupleStartN)
      pure args1
        { st = st
            { startC = st.startC + 1
            , startN = st.startN + 1
            }
        }
    else if Fn.runFn2 eqNullable nameEndC nameEndN then do
      EFn.runEffectFn3 patch.update
        args.context
        (nonNull tupleEndC)
        (nonNull tupleEndN)
      pure args1
        { st = st
            { endC = st.endC - 1
            , endN = st.endN - 1
            }
        }
    else if Fn.runFn2 eqNullable nameStartC nameEndN then do
      EFn.runEffectFn3 patch.update
        args.context
        (nonNull tupleStartC)
        (nonNull tupleEndN)
      pure args1
        { st = st
            { startC = st.startC + 1
            , endN = st.endN - 1
            }
        }
    else if Fn.runFn2 eqNullable nameEndC nameStartN then do
      EFn.runEffectFn3 patch.update
        args.context
        (nonNull tupleEndC)
        (nonNull tupleStartN)
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
  , ntoi :: MObject Int
  }

type Diff2Args ctx a =
  { patch :: Patch ctx a
  , args :: DiffArgs ctx a
  , st :: Diff2State
  }

diff2 :: forall ctx a. EFn.EffectFn1 (Diff2Args ctx a) Unit
diff2 = EFn.mkEffectFn1 \{ patch, args, st } -> do
  EFn.runEffectFn3 forE st.startN (st.endN + 1)
    $ EFn.mkEffectFn1 \idx -> do
        let tupleN = Fn.runFn2 byIdx args.nexts idx
            nameN = fst tupleN
        mIdxC <- EFn.runEffectFn2 MO.get nameN st.ntoi
        case mIdxC of
          Nothing ->
            EFn.runEffectFn2 patch.create args.context tupleN
          Just idxC -> do
            let tupleC = Fn.runFn2 byIdx args.currents idxC
            EFn.runEffectFn3 patch.update
              args.context
              tupleC
              tupleN
            EFn.runEffectFn2 MO.del nameN st.ntoi

  is <- EFn.runEffectFn1 MO.values st.ntoi
  EFn.runEffectFn2 foreachE is $ EFn.mkEffectFn1 \idxC -> do
    let tupleC = Fn.runFn2 byIdx args.currents idxC
    EFn.runEffectFn2 patch.delete args.context tupleC

nameToIdx
  :: forall ctx a
   . EFn.EffectFn1 (Diff1Args ctx a) (MObject Int)
nameToIdx = EFn.mkEffectFn1 \{ args, st } ->
  if st.startC > st.endC
    then MO.new
    else do
      ntoi <- MO.new
      EFn.runEffectFn3 forE st.startC (st.endC + 1) $ EFn.mkEffectFn1 \idx -> do
        let name = fst (Fn.runFn2 byIdx args.currents idx)
        EFn.runEffectFn3 MO.set name idx ntoi
      pure ntoi
