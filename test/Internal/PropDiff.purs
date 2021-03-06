module Test.Internal.PropDiff where

import Prelude

import Data.Array (catMaybes, cons, sort)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_, new, read)
import Effect.Uncurried as EFn
import Grain.Internal.PropDiff (Create, Delete, Patch, Update, diff)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

testPropDiff :: TestSuite
testPropDiff = suite "PropDiff" do
  suite "diff" do
    for_ targetLists \targetList ->
      test (show startingList <> " -> " <> show targetList) do
        ref <- liftEffect $ new startingList
        liftEffect $ EFn.runEffectFn2 diff patch
          { context: ref
          , currents: startingList
          , nexts: targetList
          }
        sourceList <- liftEffect $ read ref
        Assert.equal (sort targetList) (sort sourceList)
    test ("[] -> " <> show startingList) do
      ref <- liftEffect $ new []
      liftEffect $ EFn.runEffectFn2 diff patch
        { context: ref
        , currents: []
        , nexts: startingList
        }
      sourceList <- liftEffect $ read ref
      Assert.equal (sort startingList) (sort sourceList)

startingList :: Array (Tuple String String)
startingList =
  [ "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]

targetLists :: Array (Array (Tuple String String))
targetLists =
  [ []
  -- Same list
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  -- Same element: reversed
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "1", "4" /\ "2", "3" /\ "3", "2" /\ "4", "1" /\ "5", "0" /\ "6" ]
  -- Same element: moved
  , [ "5" /\ "5", "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5", "0" /\ "0" ]
  , [ "0" /\ "0", "5" /\ "5", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "0" /\ "0", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "5" /\ "5", "2" /\ "2", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "3" /\ "3", "0" /\ "0", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "5" /\ "5", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "0" /\ "0", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "5" /\ "5", "4" /\ "4" ]
  , [ "1" /\ "1", "0" /\ "0", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "1" /\ "10", "0" /\ "0", "2" /\ "20", "3" /\ "30", "4" /\ "40", "5" /\ "50" ]
  -- Less size: same order
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "1" /\ "5", "2" /\ "4", "3" /\ "3", "4" /\ "2", "5" /\ "1" ]
  -- Less size: reversed
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "4" /\ "40", "3" /\ "3", "2" /\ "20", "1" /\ "1", "0" /\ "0" ]
  -- Less size: moved
  , [ "5" /\ "5", "0" /\ "0", "1" /\ "1", "2" /\ "2", "4" /\ "4" ]
  , [ "1" /\ "1", "3" /\ "3", "4" /\ "4", "5" /\ "5", "0" /\ "0" ]
  , [ "0" /\ "0", "5" /\ "5", "1" /\ "1", "2" /\ "2", "3" /\ "3" ]
  , [ "2" /\ "2", "3" /\ "3", "4" /\ "4", "0" /\ "0", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "5" /\ "5", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "0" /\ "0", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "5" /\ "5", "3" /\ "3" ]
  , [ "2" /\ "2", "0" /\ "0", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "2" /\ "2", "3" /\ "3", "5" /\ "5", "4" /\ "4" ]
  , [ "1" /\ "1", "0" /\ "0", "2" /\ "2", "3" /\ "3", "5" /\ "5" ]
  , [ "1" /\ "0", "0" /\ "1", "2" /\ "3", "3" /\ "2", "5" /\ "10" ]
  -- Less size: discontinuous
  , [ "0" /\ "0", "1" /\ "1", "3" /\ "3", "5" /\ "5" ]
  , [ "0" /\ "0", "5" /\ "5", "1" /\ "1", "3" /\ "3" ]
  , [ "1" /\ "1", "3" /\ "3", "0" /\ "0", "5" /\ "5" ]
  , [ "5" /\ "5", "3" /\ "3", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "100", "3" /\ "2", "1" /\ "1", "0" /\ "0" ]
  -- More size: same order
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5", "6" /\ "6" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "6" /\ "6", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "6" /\ "6", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "6" /\ "6", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "6" /\ "6", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "6" /\ "6", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "6" /\ "6", "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "6" /\ "6", "0" /\ "00", "1" /\ "01", "2" /\ "02", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  -- More size: reversed
  , [ "6" /\ "6", "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "6" /\ "6", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "6" /\ "6", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "6" /\ "6", "2" /\ "2", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "6" /\ "6", "1" /\ "1", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "6" /\ "6", "0" /\ "0" ]
  , [ "5" /\ "5", "4" /\ "4", "3" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0", "6" /\ "6" ]
  , [ "5" /\ "5", "4" /\ "4", "30" /\ "3", "2" /\ "2", "1" /\ "1", "0" /\ "0", "6" /\ "6" ]
  -- More size: moved
  , [ "5" /\ "5", "0" /\ "0", "1" /\ "1", "2" /\ "2", "3" /\ "3", "6" /\ "6", "4" /\ "4" ]
  , [ "1" /\ "1", "6" /\ "6", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5", "0" /\ "0" ]
  , [ "0" /\ "0", "5" /\ "5", "1" /\ "1", "2" /\ "2", "3" /\ "3", "6" /\ "6", "4" /\ "4" ]
  , [ "6" /\ "6", "1" /\ "1", "2" /\ "2", "3" /\ "3", "4" /\ "4", "0" /\ "0", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "6" /\ "6", "2" /\ "2", "5" /\ "5", "3" /\ "3", "4" /\ "4" ]
  , [ "1" /\ "1", "2" /\ "2", "0" /\ "0", "6" /\ "6", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "5" /\ "5", "3" /\ "3", "4" /\ "4", "6" /\ "6" ]
  , [ "1" /\ "1", "6" /\ "6", "2" /\ "2", "0" /\ "0", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "6" /\ "6", "2" /\ "2", "3" /\ "3", "5" /\ "5", "4" /\ "4" ]
  , [ "1" /\ "1", "0" /\ "0", "2" /\ "2", "3" /\ "3", "6" /\ "6", "4" /\ "4", "5" /\ "5" ]
  , [ "1" /\ "1", "0" /\ "0", "2" /\ "2", "3" /\ "3", "6" /\ "600", "4" /\ "4", "5" /\ "5" ]
  -- More size: discontinuous
  , [ "0" /\ "0", "1" /\ "1", "6" /\ "6", "7" /\ "7", "2" /\ "2", "3" /\ "3", "8" /\ "8", "4" /\ "4", "5" /\ "5", "9" /\ "9" ]
  , [ "6" /\ "6", "7" /\ "7", "8" /\ "8", "0" /\ "0", "1" /\ "1", "9" /\ "9", "2" /\ "2", "3" /\ "3", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "1", "2" /\ "2", "6" /\ "6", "3" /\ "3", "7" /\ "7", "8" /\ "8", "9" /\ "9", "4" /\ "4", "5" /\ "5" ]
  , [ "0" /\ "0", "1" /\ "11", "2" /\ "2", "6" /\ "6", "3" /\ "3", "7" /\ "7", "8" /\ "88", "9" /\ "9", "4" /\ "4", "5" /\ "55" ]
  -- complex
  , [ "0" /\ "0", "3" /\ "3", "4" /\ "4", "6" /\ "6", "8" /\ "8", "9" /\ "9" ]
  , [ "9" /\ "9", "8" /\ "8", "6" /\ "6", "4" /\ "4", "3" /\ "3", "0" /\ "0" ]
  , [ "100" /\ "100"
    , "101" /\ "101"
    , "102" /\ "102"
    , "0" /\ "0"
    , "1" /\ "1"
    , "2" /\ "2"
    , "3000" /\ "3000"
    , "500" /\ "500"
    , "34" /\ "34"
    , "23" /\ "23"
    ]
  ]

upd
  :: Tuple String String
  -> Tuple String String
  -> Tuple String String
upd next@(Tuple nameN _) current@(Tuple nameC _)
  | nameN == nameC = next
  | otherwise = current

del
  :: Tuple String String
  -> Tuple String String
  -> Maybe (Tuple String String)
del (Tuple nameN _) current@(Tuple nameC _)
  | nameN == nameC = Nothing
  | otherwise = Just current

patch :: Patch (Ref (Array (Tuple String String))) String
patch = { create, delete, update }

create :: Create (Ref (Array (Tuple String String))) String
create = EFn.mkEffectFn2 \list next ->
  modify_ (cons next) list

delete :: Delete (Ref (Array (Tuple String String))) String
delete = EFn.mkEffectFn2 \list current ->
  modify_ (catMaybes <<< map (del current)) list

update :: Update (Ref (Array (Tuple String String))) String
update = EFn.mkEffectFn3 \list _ next ->
  modify_ (map (upd next)) list
