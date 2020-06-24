module Test.UI.Diff where

import Prelude

import Control.Safely as Safe
import Data.Array (deleteAt, insertAt, (!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Grain.Markup as H
import Grain.UI.Diff (diff, getKey)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

testDiff :: TestSuite
testDiff = suite "Diff" do
  test "getKey" do
    Assert.equal "text_0" $ getKey 0 $ H.text "test text"
    Assert.equal "element_span_1" $ getKey 1 $ H.span
    Assert.equal "element_span_key1" $ getKey 1 $ H.key "key1" $ H.span
    Assert.equal "component_key2" $ getKey 2 $ H.key "key2" $ H.component $ pure H.span
  suite "diff" do
    Safe.for_ targetLists \targetList ->
      test (show startingList <> " -> " <> show targetList) do
        ref <- liftEffect $ new startingList
        liftEffect $ diff patch
          { context: unit
          , parent: ref
          , currentChildren: startingList
          , nextChildren: targetList
          }
        sourceList <- liftEffect $ read ref
        Assert.equal targetList sourceList
    test ("[] -> " <> show startingList) do
      ref <- liftEffect $ new []
      liftEffect $ diff patch
        { context: unit
        , parent: ref
        , currentChildren: []
        , nextChildren: startingList
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

type PatchArgs =
  { context :: Unit
  , current :: Maybe Int
  , next :: Maybe Int
  , parentNode :: Ref (Array Int)
  , nodeIndex :: Int
  , moveIndex :: Maybe Int
  }

patch :: PatchArgs -> Effect Unit
patch x = do
  list <- read x.parentNode
  case x.current, x.next of
    Nothing, Nothing -> pure unit
    Nothing, Just num ->
      case insertAt x.nodeIndex num list of
        Nothing -> pure unit
        Just list' -> write list' x.parentNode
    Just _, Nothing ->
      case deleteAt x.nodeIndex list of
        Nothing -> pure unit
        Just list' -> write list' x.parentNode
    Just _, Just _ ->
      case x.moveIndex of
        Nothing -> pure unit
        Just i ->
          case list !! x.nodeIndex of
            Nothing -> pure unit
            Just item ->
              case deleteAt x.nodeIndex list of
                Nothing -> pure unit
                Just list' ->
                  case insertAt i item list' of
                    Nothing -> pure unit
                    Just list_ -> write list_ x.parentNode
