module Test.Internal.Diff where

import Prelude

import Data.Array (delete, insertAt)
import Data.Foldable (for_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify, modify_, new, read, write)
import Effect.Uncurried as EFn
import Grain.Internal.Diff (PatchArgs(..), diff)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

testDiff :: TestSuite
testDiff = suite "Diff" do
  suite "diff" do
    for_ targetLists \targetList ->
      test (show startingList <> " -> " <> show targetList) do
        ref <- liftEffect $ new startingList
        liftEffect $ EFn.runEffectFn3 diff getKey patch
          { context: unit
          , parentNode: ref
          , currents: startingList
          , nexts: targetList
          }
        sourceList <- liftEffect $ read ref
        Assert.equal targetList sourceList
    test ("[] -> " <> show startingList) do
      ref <- liftEffect $ new []
      liftEffect $ EFn.runEffectFn3 diff getKey patch
        { context: unit
        , parentNode: ref
        , currents: []
        , nexts: startingList
        }
      sourceList <- liftEffect $ read ref
      Assert.equal startingList sourceList

startingList :: Array Int
startingList = [ 0, 1, 2, 3, 4, 5 ]

targetLists :: Array (Array Int)
targetLists =
  [ []
  -- Same list
  , [ 0, 1, 2, 3, 4, 5 ]
  -- Same element: reversed
  , [ 5, 4, 3, 2, 1, 0 ]
  -- Same element: moved
  , [ 5, 0, 1, 2, 3, 4 ]
  , [ 1, 2, 3, 4, 5, 0 ]
  , [ 0, 5, 1, 2, 3, 4 ]
  , [ 1, 2, 3, 4, 0, 5 ]
  , [ 0, 1, 5, 2, 3, 4 ]
  , [ 1, 2, 3, 0, 4, 5 ]
  , [ 0, 1, 2, 5, 3, 4 ]
  , [ 1, 2, 0, 3, 4, 5 ]
  , [ 0, 1, 2, 3, 5, 4 ]
  , [ 1, 0, 2, 3, 4, 5 ]
  -- Less size: same order
  , [ 0, 1, 2, 3, 4 ]
  , [ 0, 1, 2, 3, 5 ]
  , [ 0, 1, 2, 4, 5 ]
  , [ 0, 1, 3, 4, 5 ]
  , [ 0, 2, 3, 4, 5 ]
  , [ 1, 2, 3, 4, 5 ]
  -- Less size: reversed
  , [ 5, 4, 3, 2, 1 ]
  , [ 5, 4, 3, 2, 0 ]
  , [ 5, 4, 3, 1, 0 ]
  , [ 5, 4, 2, 1, 0 ]
  , [ 5, 3, 2, 1, 0 ]
  , [ 4, 3, 2, 1, 0 ]
  -- Less size: moved
  , [ 5, 0, 1, 2, 4 ]
  , [ 1, 3, 4, 5, 0 ]
  , [ 0, 5, 1, 2, 3 ]
  , [ 2, 3, 4, 0, 5 ]
  , [ 0, 1, 5, 3, 4 ]
  , [ 1, 2, 0, 4, 5 ]
  , [ 0, 1, 2, 5, 3 ]
  , [ 2, 0, 3, 4, 5 ]
  , [ 0, 2, 3, 5, 4 ]
  , [ 1, 0, 2, 3, 5 ]
  -- Less size: discontinuous
  , [ 0, 1, 3, 5 ]
  , [ 0, 5, 1, 3 ]
  , [ 1, 3, 0, 5 ]
  , [ 5, 3, 1, 0 ]
  -- More size: same order
  , [ 0, 1, 2, 3, 4, 5, 6 ]
  , [ 0, 1, 2, 3, 4, 6, 5 ]
  , [ 0, 1, 2, 3, 6, 4, 5 ]
  , [ 0, 1, 2, 6, 3, 4, 5 ]
  , [ 0, 1, 6, 2, 3, 4, 5 ]
  , [ 0, 6, 1, 2, 3, 4, 5 ]
  , [ 6, 0, 1, 2, 3, 4, 5 ]
  -- More size: reversed
  , [ 6, 5, 4, 3, 2, 1, 0 ]
  , [ 5, 6, 4, 3, 2, 1, 0 ]
  , [ 5, 4, 6, 3, 2, 1, 0 ]
  , [ 5, 4, 3, 6, 2, 1, 0 ]
  , [ 5, 4, 3, 2, 6, 1, 0 ]
  , [ 5, 4, 3, 2, 1, 6, 0 ]
  , [ 5, 4, 3, 2, 1, 0, 6 ]
  -- More size: moved
  , [ 5, 0, 1, 2, 3, 6, 4 ]
  , [ 1, 6, 2, 3, 4, 5, 0 ]
  , [ 0, 5, 1, 2, 3, 6, 4 ]
  , [ 6, 1, 2, 3, 4, 0, 5 ]
  , [ 0, 1, 6, 2, 5, 3, 4 ]
  , [ 1, 2, 0, 6, 3, 4, 5 ]
  , [ 0, 1, 2, 5, 3, 4, 6 ]
  , [ 1, 6, 2, 0, 3, 4, 5 ]
  , [ 0, 1, 6, 2, 3, 5, 4 ]
  , [ 1, 0, 2, 3, 6, 4, 5 ]
  -- More size: discontinuous
  , [ 0, 1, 6, 7, 2, 3, 8, 4, 5, 9 ]
  , [ 6, 7, 8, 0, 1, 9, 2, 3, 4, 5 ]
  , [ 0, 1, 2, 6, 3, 7, 8, 9, 4, 5 ]
  -- complex
  , [ 0, 3, 4, 6, 8, 9 ]
  , [ 9, 8, 6, 4, 3, 0 ]
  , [ 100, 101, 102, 0, 1, 2, 3000, 500, 34, 23 ]
  ]

getKey :: Fn.Fn2 Int Int String
getKey = Fn.mkFn2 \_ i -> show i

patch :: EFn.EffectFn1 (PatchArgs Unit (Ref (Array Int)) Int) Unit
patch = EFn.mkEffectFn1 \act ->
  case act of
    Create { parentNode, index, next } -> do
      list <- read parentNode
      case insertAt index next list of
        Nothing -> pure unit
        Just list' -> write list' parentNode

    Delete { parentNode, current } ->
      modify_ (delete current) parentNode

    Move { parentNode, index, current, next } -> do
      list <- modify (delete current) parentNode
      case insertAt index next list of
        Nothing -> pure unit
        Just list' -> write list' parentNode

    _ ->
      pure unit
