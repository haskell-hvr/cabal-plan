{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Test.Tasty

import qualified Cabal.PlanTest

main :: IO ()
main =
    defaultMain
      (testGroup "Tests"
        [ Cabal.PlanTest.test ])
