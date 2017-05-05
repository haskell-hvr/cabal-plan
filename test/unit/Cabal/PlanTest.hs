{-# LANGUAGE OverloadedStrings          #-}

module Cabal.PlanTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Cabal.Plan

test :: TestTree
test = testGroup "Cabal.Plan" tests

tests :: [TestTree]
tests =
    [ testGroup "dispVer"
        [ testCase "examples"
            (do
              dispVer (Ver [1, 2, 3, 4]) @?= "1.2.3.4")]]
