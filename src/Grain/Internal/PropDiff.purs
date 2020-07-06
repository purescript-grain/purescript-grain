module Grain.Internal.PropDiff
  ( PatchArgs(..)
  , diff
  ) where

import Prelude

import Data.Array (length)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Effect.Uncurried as EFn
import Grain.Internal.Effect (forE, foreachE, tailRecE)
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Util (byIdx, byIdxNullable, eqNullable, mapNullable, nonNull)

data PatchArgs a
  = Create
      { next :: Tuple String a
      }
  | Delete
      { current :: Tuple String a
      }
  | Update
      { current :: Tuple String a
      , next :: Tuple String a
      }

type Patch a =
  EFn.EffectFn1 (PatchArgs a) Unit

type DiffArgs a =
  { currents :: Array (Tuple String a)
  , nexts :: Array (Tuple String a)
  }

diff
  :: forall a
   . EFn.EffectFn2 (Patch a) (DiffArgs a) Unit
diff = EFn.mkEffectFn2 \patch args@{ currents, nexts } -> do
  let lengthC = length currents
      lengthN = length nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let next = Fn.runFn2 byIdx nexts i
        EFn.runEffectFn1 patch $ Create { next }
    l, 0 ->
      EFn.runEffectFn3 forE 0 l $ EFn.mkEffectFn1 \i -> do
        let current = Fn.runFn2 byIdx currents i
        EFn.runEffectFn1 patch $ Delete { current }
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
      void $ EFn.runEffectFn2 tailRecE diff2
        { done: false
        , patch
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

type Diff1Args a =
  { done :: Boolean
  , patch :: Patch a
  , args :: DiffArgs a
  , st :: Diff1State
  }

diff1 :: forall a. EFn.EffectFn1 (Diff1Args a) (Diff1Args a)
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

        eqStart = Fn.runFn2 eqNullable nameStartC nameStartN
        eqEnd = Fn.runFn2 eqNullable nameEndC nameEndN
        eqStartEnd = Fn.runFn2 eqNullable nameStartC nameEndN
        eqEndStart = Fn.runFn2 eqNullable nameEndC nameStartN

    if eqStart then do
      EFn.runEffectFn1 patch $ Update
        { current: nonNull tupleStartC
        , next: nonNull tupleStartN
        }
      pure args1
        { st = st
            { startC = st.startC + 1
            , startN = st.startN + 1
            }
        }
    else if eqEnd then do
      EFn.runEffectFn1 patch $ Update
        { current: nonNull tupleEndC
        , next: nonNull tupleEndN
        }
      pure args1
        { st = st
            { endC = st.endC - 1
            , endN = st.endN - 1
            }
        }
    else if eqStartEnd then do
      EFn.runEffectFn1 patch $ Update
        { current: nonNull tupleStartC
        , next: nonNull tupleEndN
        }
      pure args1
        { st = st
            { startC = st.startC + 1
            , endN = st.endN - 1
            }
        }
    else if eqEndStart then do
      EFn.runEffectFn1 patch $ Update
        { current: nonNull tupleEndC
        , next: nonNull tupleStartN
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
  , ntoi :: MObject Int
  }

type Diff2Args a =
  { done :: Boolean
  , patch :: Patch a
  , args :: DiffArgs a
  , st :: Diff2State
  }

diff2 :: forall a. EFn.EffectFn1 (Diff2Args a) (Diff2Args a)
diff2 = EFn.mkEffectFn1 \args2@{ patch, args, st } ->
  if st.startN <= st.endN then do
    let tupleN = Fn.runFn2 byIdx args.nexts st.startN
        nameN = fst tupleN
    mIdxC <- EFn.runEffectFn2 MO.get nameN st.ntoi
    case mIdxC of
      Nothing -> do
        EFn.runEffectFn1 patch $ Create { next: tupleN }
        pure args2
          { st = st
              { startN = st.startN + 1
              }
          }
      Just idxC -> do
        let tupleC = Fn.runFn2 byIdx args.currents idxC
        EFn.runEffectFn1 patch $ Update
          { current: tupleC
          , next: tupleN
          }
        EFn.runEffectFn2 MO.del nameN st.ntoi
        pure args2
          { st = st { startN = st.startN + 1 }
          }
  else if MO.unsafeSize st.ntoi > 0 then do
    ns <- EFn.runEffectFn1 MO.keys st.ntoi
    EFn.runEffectFn2 foreachE ns $ EFn.mkEffectFn1 \nameC -> do
      idxC <- EFn.runEffectFn2 MO.unsafeGet nameC st.ntoi
      let tupleC = Fn.runFn2 byIdx args.currents idxC
      EFn.runEffectFn1 patch $ Delete { current: tupleC }
    pure args2 { done = true }
  else
    pure args2 { done = true }

nameToIdx
  :: forall a
   . EFn.EffectFn1 (Diff1Args a) (MObject Int)
nameToIdx = EFn.mkEffectFn1 \{ args, st } ->
  if st.startC > st.endC
    then MO.new
    else do
      ntoi <- MO.new
      EFn.runEffectFn3 forE st.startC (st.endC + 1) $ EFn.mkEffectFn1 \idx -> do
        let name = fst (Fn.runFn2 byIdx args.currents idx)
        EFn.runEffectFn3 MO.set name idx ntoi
      pure ntoi
