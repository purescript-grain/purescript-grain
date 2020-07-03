module Grain.UI.PropDiff
  ( PatchArgs(..)
  , diff
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Grain.Effect (forE, foreachE)
import Grain.MObject (MObject)
import Grain.MObject as MO
import Grain.UI.Util (index_, just)

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
  PatchArgs a -> Effect Unit

type DiffArgs a =
  { currents :: Array (Tuple String a)
  , nexts :: Array (Tuple String a)
  }

diff
  :: forall a
   . Patch a
  -> DiffArgs a
  -> Effect Unit
diff patch args@{ currents, nexts } = do
  let lengthC = length currents
      lengthN = length nexts
  case lengthC, lengthN of
    0, 0 ->
      pure unit
    0, l ->
      forE 0 l \i -> do
        let next = index_ nexts i
        patch $ Create { next }
    l, 0 ->
      forE 0 l \i -> do
        let current = index_ currents i
        patch $ Delete { current }
    _, _ -> do
      args1 <- tailRecM diff1
        { patch
        , args
        , st:
            { startC: 0
            , endC: lengthC - 1
            , startN: 0
            , endN: lengthN - 1
            }
        }
      ntoi <- nameToIdx args1
      tailRecM diff2
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

type Diff1Args a =
  { patch :: Patch a
  , args :: DiffArgs a
  , st :: Diff1State
  }

diff1
  :: forall a
   . Diff1Args a
  -> Effect (Step (Diff1Args a) (Diff1Args a))
diff1 args1@{ patch, args, st }
  | st.startC > st.endC =
      pure $ Done args1
  | st.startN > st.endN =
      pure $ Done args1
  | otherwise = do
      let tupleStartC = args.currents !! st.startC
          tupleEndC = args.currents !! st.endC
          tupleStartN = args.nexts !! st.startN
          tupleEndN = args.nexts !! st.endN

          nameStartC = fst <$> tupleStartC
          nameEndC = fst <$> tupleEndC
          nameStartN = fst <$> tupleStartN
          nameEndN = fst <$> tupleEndN

          eqStart = eqName nameStartC nameStartN
          eqEnd = eqName nameEndC nameEndN
          eqStartEnd = eqName nameStartC nameEndN
          eqEndStart = eqName nameEndC nameStartN

      if eqStart then do
        patch $ Update
          { current: just tupleStartC
          , next: just tupleStartN
          }
        pure $ Loop args1
          { st = st
              { startC = st.startC + 1
              , startN = st.startN + 1
              }
          }
      else if eqEnd then do
        patch $ Update
          { current: just tupleEndC
          , next: just tupleEndN
          }
        pure $ Loop args1
          { st = st
              { endC = st.endC - 1
              , endN = st.endN - 1
              }
          }
      else if eqStartEnd then do
        patch $ Update
          { current: just tupleStartC
          , next: just tupleEndN
          }
        pure $ Loop args1
          { st = st
              { startC = st.startC + 1
              , endN = st.endN - 1
              }
          }
      else if eqEndStart then do
        patch $ Update
          { current: just tupleEndC
          , next: just tupleStartN
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
  , ntoi :: MObject Int
  }

type Diff2Args a =
  { patch :: Patch a
  , args :: DiffArgs a
  , st :: Diff2State
  }

diff2
  :: forall a
   . Diff2Args a
  -> Effect (Step (Diff2Args a) Unit)
diff2 args2@{ patch, args, st }
  | st.startN <= st.endN = do
      let tupleN = index_ args.nexts st.startN
          nameN = fst tupleN
      mIdxC <- MO.get nameN st.ntoi
      case mIdxC of
        Nothing -> do
          patch $ Create { next: tupleN }
          pure $ Loop args2
            { st = st
                { startN = st.startN + 1
                }
            }
        Just idxC -> do
          let tupleC = index_ args.currents idxC
          patch $ Update
            { current: tupleC
            , next: tupleN
            }
          MO.del nameN st.ntoi
          pure $ Loop args2
            { st = st { startN = st.startN + 1 }
            }
  | unsafePerformEffect (MO.size st.ntoi) > 0 = do
      ns <- MO.keys st.ntoi
      foreachE ns \nameC -> do
        idxC <- MO.unsafeGet nameC st.ntoi
        let tupleC = index_ args.currents idxC
        patch $ Delete { current: tupleC }
      pure $ Done unit
  | otherwise =
      pure $ Done unit

nameToIdx
  :: forall a
   . Diff1Args a
  -> Effect (MObject Int)
nameToIdx { args: { currents }, st: { startC, endC } }
  | startC > endC = MO.new
  | otherwise = do
      ntoi <- MO.new
      forE startC (endC + 1) \idx ->
        MO.set (fst $ index_ currents idx) idx ntoi
      pure ntoi

eqName :: Maybe String -> Maybe String -> Boolean
eqName (Just c) (Just n) = c == n
eqName _ _ = false
