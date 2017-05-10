{-# LANGUAGE OverloadedStrings          #-}

module Cabal.PlanTest where

import           Data.Aeson
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
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
              dispPkgId (PkgId (PkgName "pkg1") (Ver [1, 2])) @?= "pkg1-1.2")]
    , testGroup "planJsonIdGraph"
        [ testCase "examples"
            (do
              planJsonIdGraph (mkPlanJson [(unitId1, unit1)])
                @?= M.fromList [(unitId1, S.fromList [unitId2])])]
    , testGroup "planJsonIdRoots"
        [ testCase "examples"
            (do
              planJsonIdRoots (mkPlanJson [(unitId1, unit1)])
                @?= S.fromList [unitId1])]
    , testGroup "FromJSON Ver"
        [ testCase "examples"
            (do
              eitherDecode "\"1.2.3\"" @?= Right (Ver [1, 2, 3]))]]

-- fromjson: pkgid, compname, compinfo, unit, planjson, sha256
-- add Sha example; dispSha256
-- int test: findAndDecodePlanJson

unitId1 :: UnitId
unit1 :: Unit
(unitId1, unit1) = mkLocalLibUnit "pkg1" [1,0] "hsh1" [unitId2]

unitId2 :: UnitId
unitId2 = mkUnitId "pkg2" [2,0] "hsh2"

mkUnitId :: Text -> [Int] -> Text -> UnitId
mkUnitId name ver hsh =
  UnitId
    (name `T.append` "-" `T.append` (dispVer (Ver ver)) `T.append` "-" `T.append` hsh)

mkLocalLibUnit :: Text -> [Int] -> Text -> [UnitId] -> (UnitId, Unit)
mkLocalLibUnit name ver hsh libDeps =
  let unitId = mkUnitId name ver hsh
  in
    ( unitId
    , Unit
        { uId = unitId
        , uPId = PkgId (PkgName name) (Ver ver)
        , uType = UnitTypeLocal
        , uSha256 = Nothing
        , uComps = M.fromList [(CompNameLib, mkLibsCompInfo libDeps)]
        , uFlags = M.fromList []
        })

mkLibsCompInfo :: [UnitId] -> CompInfo
mkLibsCompInfo libIds =
  CompInfo
    { ciLibDeps = S.fromList libIds
    , ciExeDeps = S.fromList []
    , ciBinFile = Nothing
    }

mkPlanJson :: [(UnitId, Unit)] -> PlanJson
mkPlanJson idUnitPairs =
  PlanJson
    { pjCabalVersion = Ver [2, 0]
    , pjCabalLibVersion = Ver [2, 0]
    , pjCompilerId = PkgId (PkgName "GHC") (Ver [8, 0, 1])
    , pjArch = "X86_64"
    , pjOs = "Linux"
    , pjUnits = M.fromList idUnitPairs
    }
