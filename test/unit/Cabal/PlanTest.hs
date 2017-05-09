{-# LANGUAGE OverloadedStrings          #-}

module Cabal.PlanTest where

import qualified Data.Map                     as M
import qualified Data.Set                     as S
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
              dispVer (Ver [1, 2, 3, 4]) @?= "1.2.3.4")]
    , testGroup "dispCompName"
        [ testCase "examples"
            (do
              dispCompName CompNameLib @?= "lib")]
    , testGroup "dispPkgId"
        [ testCase "examples"
            (do
              dispPkgId (PkgId (PkgName "pkg1") (Ver [1, 2]))
                @?= "pkg1-1.2")]
    , testGroup "planJsonIdGraph"
        [ testCase "examples"
            (do
              planJsonIdGraph planJson1
                @?= M.fromList [(unit1Id, S.fromList [unit2Id])])]
    , testGroup "planJsonIdRoots"
        [ testCase "examples"
            (do
              planJsonIdRoots planJson1
                @?= S.fromList [unit1Id])]]

planJson1 :: PlanJson
planJson1 =
  PlanJson
    { pjCabalVersion = Ver [1, 24]
    , pjCabalLibVersion = Ver [1, 24]
    , pjCompilerId = PkgId (PkgName "GHC") (Ver [8, 0, 1])
    , pjArch = "X86_64"
    , pjOs = "Linux"
    , pjUnits = M.fromList [(unit1Id, unit1)]
    }

unit1Id :: UnitId
unit1Id = UnitId "pkg1-1.0-hsh1"

unit1 :: Unit
unit1 =
  Unit
    { uId = unit1Id
    , uPId = PkgId (PkgName "pkg1") (Ver [1, 0])
    , uType = UnitTypeLocal
    , uSha256 = Nothing
    , uComps = M.fromList [(CompNameLib, compInfo1)]
    , uFlags = M.fromList []
    }

unit2Id :: UnitId
unit2Id = UnitId "pkg2-2.0-hsh2"

compInfo1 :: CompInfo
compInfo1 =
  CompInfo
    { ciLibDeps =
        S.fromList [ unit2Id ]
    , ciExeDeps =
        S.fromList []
    , ciBinFile = Nothing
    }

-- individual parsers, encoders, including PlanJson
-- findAndDecodePlanJson
-- add Sha example; dispSha256
