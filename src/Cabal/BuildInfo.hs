{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | SPDX-License-Identifier: GPL-2.0-or-later
--
-- Utilities for reading @cabal@'s @show-build-info@ output
--
-- @plan.json@ are generated when using @cabal@
-- <http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html Nix-style Local Builds>.
module Cabal.BuildInfo
    ( BuildInfoJson(..)
    , CompBuildInfo(..)
    , ModuleName(..)
    , CompilerFlavor(..)

    -- * Basic types
    , Ver(..)
    , PkgId(..)
    , dispPkgId
    , UnitId(..)

    , decodeBuildInfoJson
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString              as B
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Text                    (Text)

import           Cabal.Plan

-- | Equivalent to @Cabal@'s @Distribution.ModuleName.ModuleName@
newtype ModuleName = ModuleName Text
                deriving (Show,Eq,Ord,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

-- | Corresponding to @Cabal@'s @Distribution.Compiler.CompilerFlavor@
data CompilerFlavor =
  GHC | GHCJS | NHC | YHC | Hugs | HBC | Helium | JHC | LHC | UHC | Eta
  | OtherCompiler !Text
    deriving (Show,Eq,Ord)

instance FromJSON CompilerFlavor where
    parseJSON = withText "CompilerFlavor" $ \flavor -> return $
        case flavor of
            "ghc"    -> GHC
            "ghcjs"  -> GHCJS
            "nhc98"  -> NHC
            "yhc"    -> YHC
            "hugs"   -> Hugs
            "hbc"    -> HBC
            "helium" -> Helium
            "jhc"    -> JHC
            "lhc"    -> LHC
            "uhc"    -> UHC
            "eta"    -> Eta
            name     -> OtherCompiler name
            -- HaskellSuite name -> name -- This one is currently ambigous

data BuildInfoJson = BuildInfoJson
    { biCabalLibVersion     :: !Ver
    , biCompierFlavor       :: !CompilerFlavor
    , biCompilerId          :: !PkgId
    , biCompilerPath        :: !FilePath
    , biComps               :: !(Map UnitId CompBuildInfo)
    } deriving Show

instance FromJSON BuildInfoJson where
    parseJSON = withObject "BuildInfo" $ \o -> do
        biCabalLibVersion  <- o .: "cabal-version"

        biComps            <- toMap =<< o .: "components"

        compiler :: Object <- o .: "compiler"
        biCompilerId       <- compiler .: "compiler-id"
        biCompierFlavor    <- compiler .: "flavour"
        biCompilerPath     <- compiler .: "path"

        pure BuildInfoJson{..}

      where
        toMap pil = do
            let pim = M.fromList [ (cbiUnitId pi',pi') | pi' <- pil ]
            unless (M.size pim == length pil) $
                fail "components[] has duplicate unit-ids"
            pure pim

data CompBuildInfo = CompBuildInfo
    { cbiName         :: !CompName
    , cbiUnitId       :: !UnitId
    , cbiCompilerArgs :: ![String]
    , cbiModules      :: ![ModuleName]
    , cbiSourceFiles  :: ![FilePath]
    , cbiSourceDirs   :: ![FilePath]
    } deriving Show

instance FromJSON CompBuildInfo where
    parseJSON = withObject "CompBuildInfo" $ \o -> do
        cbiName           <- o .: "name"
        cbiUnitId         <- o .: "unit-id"
        cbiCompilerArgs   <- o .: "compiler-args"
        cbiModules        <- o .: "modules"
        cbiSourceFiles    <- o .: "src-files"
        cbiSourceDirs     <- o .: "src-dirs"
        pure CompBuildInfo{..}

decodeBuildInfoJson :: FilePath -> IO PlanJson
decodeBuildInfoJson buildInfoJsonFn = do
    jsraw <- B.readFile buildInfoJsonFn
    either fail pure $ eitherDecodeStrict' jsraw
