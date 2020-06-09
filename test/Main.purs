module Test.Main where

import Prelude

import Effect (Effect)
import Test.UI.Diff (testDiff)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  testDiff
