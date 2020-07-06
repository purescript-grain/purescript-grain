module Test.Main where

import Prelude

import Effect (Effect)
import Test.Internal.Diff (testDiff)
import Test.Internal.PropDiff (testPropDiff)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  testDiff
  testPropDiff
