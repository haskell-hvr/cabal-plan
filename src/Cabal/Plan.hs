{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Utilities for reading @cabal@'s @plan.json@ file
--
-- @plan.json@ are generated when using @cabal@
-- <http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html Nix-style Local Builds>.
module Cabal.Plan
    (
      PlanJson(..)
    , Unit(..)
    , CompName(..)
    , dispCompName
    , CompInfo(..)
    , UnitType(..)

    -- * Basic types
    , Ver(..)
    , dispVer
    , PkgName(..)
    , PkgId(..)
    , dispPkgId
    , UnitId(..)
    , FlagName(..)

    -- ** SHA-256
    , Sha256
    , dispSha256
    , parseSha256
    , sha256ToByteString
    , sha256FromByteString

    -- * Utilities
    , planJsonIdGraph
    , planJsonIdRoots

    -- * Convenience functions
    , findAndDecodePlanJson
    , decodePlanJson
    ) where

import           Control.Applicative          as App
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base16       as B16
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Version                 as DV
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

----------------------------------------------------------------------------

-- | Equivalent to @Cabal@'s @Distribution.Package.Version@
newtype Ver = Ver [Int]
            deriving (Show,Eq,Ord)

-- | Equivalent to @Cabal@'s @Distribution.Package.UnitId@
newtype UnitId = UnitId Text
               deriving (Show,Eq,Ord,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

-- | Equivalent to @Cabal@'s @Distribution.Package.PackageName@
newtype PkgName = PkgName Text
                deriving (Show,Eq,Ord,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

-- | Equivalent to @Cabal@'s @Distribution.Package.PackageIdentifier@
data PkgId = PkgId !PkgName !Ver
           deriving (Show,Eq,Ord)

-- | Equivalent to @Cabal@'s @Distribution.PackageDescription.FlagName@
--
-- @since 0.3.0.0
newtype FlagName = FlagName Text
                 deriving (Show,Eq,Ord,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

-- | <https://en.wikipedia.org/wiki/SHA-2 SHA-256> hash
newtype Sha256 = Sha256 B.ByteString -- internal invariant: exactly 32 bytes long
               deriving (Eq,Ord)

-- | Represents the information contained in cabal's @plan.json@ file.
--
-- This comprises basic information describing the environment as well
-- as the install/build plan computed by @cabal@.
data PlanJson = PlanJson
     { pjCabalVersion    :: !Ver                     -- ^ Version of @cabal@ frontend
     , pjCabalLibVersion :: !Ver                     -- ^ Version of Cabal library
     , pjCompilerId      :: !PkgId                   -- ^ Name and version of Haskell compiler
     , pjArch            :: !Text                    -- ^ Architecture name
     , pjOs              :: !Text                    -- ^ Operating system name
     , pjUnits           :: !(M.Map UnitId Unit) -- ^ install/build plan
     } deriving Show

-- | Describes kind of build unit and its provenance
data UnitType = UnitTypeBuiltin -- ^ Lives in global (non-nix-style) package db
              | UnitTypeGlobal  -- ^ Lives in Nix-store cache
              | UnitTypeLocal   -- ^ Local package
              | UnitTypeInplace -- ^ Local in-place package
              deriving (Show,Eq)

-- | Represents a build-plan unit uniquely identified by its 'UnitId'
data Unit = Unit
     { uId      :: !UnitId      -- ^ Unit ID uniquely identifying a 'Unit' in install plan
     , uPId     :: !PkgId       -- ^ Package name and version (not necessarily unique within plan)
     , uType    :: !UnitType      -- ^ Describes type of build item, see 'UnitType'
     , uSha256  :: !(Maybe Sha256) -- ^ SHA256 source tarball checksum (as used by e.g. @hackage-security@)
     , uComps   :: !(Map CompName CompInfo) -- ^ Components identified by 'UnitId'
       --
       -- When @cabal@ needs to fall back to legacy-mode (currently for
       -- @custom@ build-types or obsolete @cabal-version@ values), 'uComps'
       -- may contain more than one element.
     , uFlags   :: !(Map FlagName Bool) -- ^ cabal flag settings (not available for 'UnitTypeBuiltin')
     , uDistDir :: !(Maybe FilePath) -- ^ In-place dist-dir (if available)
                                     --
                                     -- @since 0.3.0.0
     } deriving Show

-- | Component name inside a build-plan unit
--
-- A similiar type exists in @Cabal@ codebase, see
-- @Distribution.Simple.LocalBuildInfo.ComponentName@
data CompName =
    CompNameLib
  | CompNameSubLib !Text
  | CompNameFLib   !Text -- ^ @since 0.3.0.0
  | CompNameExe    !Text
  | CompNameTest   !Text
  | CompNameBench  !Text
  | CompNameSetup
  deriving (Show, Eq, Ord)

-- | Describes component-specific information inside a 'Unit'
data CompInfo = CompInfo
    { ciLibDeps :: Set UnitId     -- ^ library dependencies
    , ciExeDeps :: Set UnitId     -- ^ executable dependencies
    , ciBinFile :: Maybe FilePath -- ^ path-name of artifact if available
    } deriving Show

----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- JSON instances

instance FromJSON CompName where
    parseJSON = withText "CompName" (maybe (fail "invalid CompName") pure . parseCompName)

instance ToJSON CompName where
    toJSON     = toJSON . dispCompName

instance FromJSONKey CompName where
    fromJSONKey = FromJSONKeyTextParser (maybe (fail "CompName") pure . parseCompName)

instance ToJSONKey   CompName where
    toJSONKey = toJSONKeyText dispCompName

----

instance FromJSON CompInfo where
    parseJSON = withObject "CompInfo" $ \o ->
        CompInfo <$> o .:?! "depends"
                 <*> o .:?! "exe-depends"
                 <*> o .:? "bin-file"

----

instance FromJSON PkgId where
    parseJSON = withText "PkgId" (maybe (fail "invalid PkgId") pure . parsePkgId)

instance ToJSON PkgId where
    toJSON = toJSON . dispPkgId

instance FromJSONKey PkgId where
    fromJSONKey = FromJSONKeyTextParser (maybe (fail "PkgId") pure . parsePkgId)

instance ToJSONKey PkgId where
    toJSONKey = toJSONKeyText dispPkgId


----------------------------------------------------------------------------
-- parser helpers

parseCompName :: Text -> Maybe CompName
parseCompName t0 = case T.splitOn ":" t0 of
                     ["lib"]     -> Just CompNameLib
                     ["lib",n]   -> Just $! CompNameSubLib n
                     ["flib",n]  -> Just $! CompNameFLib n
                     ["exe",n]   -> Just $! CompNameExe n
                     ["bench",n] -> Just $! CompNameBench n
                     ["test",n]  -> Just $! CompNameTest n
                     ["setup"]   -> Just CompNameSetup
                     _           -> Nothing

-- | Pretty print 'CompName'
dispCompName :: CompName -> Text
dispCompName cn = case cn of
    CompNameLib      -> "lib"
    CompNameSubLib n -> "lib:" <> n
    CompNameFLib n   -> "flib:" <> n
    CompNameExe n    -> "exe:" <> n
    CompNameBench n  -> "bench:" <> n
    CompNameTest n   -> "test:" <> n
    CompNameSetup    -> "setup"

instance FromJSON PlanJson where
    parseJSON = withObject "PlanJson" $ \o -> do
        pjCabalVersion    <- o .: "cabal-version"

        unless (pjCabalVersion >= Ver [2]) $
            fail ("plan.json version " ++ T.unpack (dispVer pjCabalVersion) ++ " not supported")

        pjCabalLibVersion <- o .: "cabal-lib-version"
        pjCompilerId      <- o .: "compiler-id"
        pjArch            <- o .: "arch"
        pjOs              <- o .: "os"
        pjUnits           <- toMap =<< o .: "install-plan"

        App.pure PlanJson{..}
      where
        toMap pil = do
            let pim = M.fromList [ (uId pi',pi') | pi' <- pil ]
            unless (M.size pim == length pil) $
                fail "install-plan[] has duplicate ids"
            pure pim

(.:?!) :: (FromJSON a, Monoid a) => Object -> Text -> Parser a
o .:?! fld = o .:? fld .!= mempty

planItemAllDeps :: Unit -> Set UnitId
planItemAllDeps Unit{..} = mconcat [ ciLibDeps <> ciExeDeps | CompInfo{..} <- M.elems uComps ]

instance FromJSON Unit where
    parseJSON = withObject "Unit" $ \o -> do
        mcomponents    <- o .:? "components"
        mcomponentname <- o .:? "component-name"
        ty             <- o .:  "type"
        mstyle         <- o .:? "style"

        uId     <- o .: "id"
        uPId    <- PkgId <$> o .: "pkg-name" <*> o .: "pkg-version"
        uType   <- case (ty :: Text, mstyle :: Maybe Text) of
                   ("pre-existing",Nothing)      -> pure UnitTypeBuiltin
                   ("configured",Just "global")  -> pure UnitTypeGlobal
                   ("configured",Just "local")   -> pure UnitTypeLocal
                   ("configured",Just "inplace") -> pure UnitTypeInplace
                   _                             -> fail (show (ty,mstyle))
        uFlags  <- o .:?! "flags"
        uSha256 <- o .:? "pkg-src-sha256"
        uComps  <- case (mcomponents, mcomponentname) of
          (Just comps0, Nothing) ->
              pure comps0
          (Nothing, Just cname) ->
              M.singleton cname <$> parseJSON (Object o)
          (Nothing, Nothing) | uType == UnitTypeBuiltin ->
              M.singleton CompNameLib <$> parseJSON (Object o)
          _ -> fail (show o)

        uDistDir <- o .:? "dist-dir"

        pure Unit{..}

----------------------------------------------------------------------------
-- Convenience helper

-- | Locates the project root for cabal project in scope for the current
-- working directory.
--
-- @plan.json@ is located from either the optional build dir argument, or in
-- the default directory (@dist-newstyle@) relative to the project root.
--
-- The folder assumed to be the project-root is returned as well.
--
-- Throws 'IO' exceptions on errors.
--
findAndDecodePlanJson
    :: Maybe FilePath -- ^ Optional build dir to look in.
    -> IO (PlanJson, FilePath)
findAndDecodePlanJson mBuildDir = do
    projbase <- findProjRoot

    let distFolder = fromMaybe (projbase </> "dist-newstyle") mBuildDir
    haveDistFolder <- doesDirectoryExist distFolder

    unless haveDistFolder $
        fail ("missing " ++ show distFolder ++ " folder; do you need to run 'cabal new-build'?")

    let planJsonFn = distFolder </> "cache" </> "plan.json"

    havePlanJson <- doesFileExist planJsonFn

    unless havePlanJson $
        fail "missing 'plan.json' file; do you need to run 'cabal new-build'?"

    plan <- decodePlanJson planJsonFn

    pure (plan, projbase)

-- | Decodes @plan.json@ file location provided as 'FilePath'
--
-- This is a trivial convenience function so that the caller doesn't
-- have to depend on @aeson@ directly
--
-- Throws 'IO' exceptions on errors.
--
decodePlanJson :: FilePath -> IO PlanJson
decodePlanJson planJsonFn = do
    jsraw <- B.readFile planJsonFn
    either fail pure $ eitherDecodeStrict' jsraw

-- Find project root, this emulates cabal's current heuristic
--
-- TODO: currently fallsback to CWD if no cabal.project is found; fallback to locating $pkg.cabal files instead
findProjRoot :: IO FilePath
findProjRoot = do
    cwd  <- getCurrentDirectory

    let tst d = do let fn = d </> "cabal.project"
                   ex <- doesFileExist fn
                   if ex then pure (Just fn) else pure Nothing

    md <- walkUpFolders tst cwd

    pure (maybe cwd fst md)


walkUpFolders :: (FilePath -> IO (Maybe a)) -> FilePath -> IO (Maybe (FilePath,a))
walkUpFolders dtest d0 = do
    home <- getHomeDirectory

    let go d | d == home  = pure Nothing
             | isDrive d  = pure Nothing
             | otherwise  = do
                   t <- dtest d
                   case t of
                     Just a  -> pure $ Just (d, a)
                     Nothing -> go (takeDirectory d)

    go d0


parseVer :: Text -> Maybe Ver
parseVer str = case reverse $ readP_to_S DV.parseVersion (T.unpack str) of
  (ver, "") : _ | not (null (DV.versionBranch ver)), all (>= 0) (DV.versionBranch ver)
      -> Just (Ver $ DV.versionBranch ver)
  _   -> Nothing

-- | Pretty print 'Ver'
dispVer :: Ver -> Text
dispVer (Ver ns) = T.pack $ intercalate "." (map show ns)

instance FromJSON Ver where
    parseJSON = withText "Ver" (maybe (fail "Ver") pure . parseVer)

instance ToJSON Ver where
    toJSON = toJSON . dispVer

parsePkgId :: Text -> Maybe PkgId
parsePkgId t = do
  let (pns_, pvs) = T.breakOnEnd "-" t
  pv <- parseVer pvs

  pn <- T.stripSuffix "-" pns_

  -- TODO: validate pn
  pure (PkgId (PkgName pn) pv)

-- | Pretty print 'PkgId'
dispPkgId :: PkgId -> Text
dispPkgId (PkgId (PkgName pn) pv) = pn <> "-" <> dispVer pv


-- | Pretty print 'Sha256' as base-16.
dispSha256 :: Sha256 -> Text
dispSha256 (Sha256 s) = T.decodeLatin1 (B16.encode s)

-- | Parse base-16 encoded 'Sha256'.
--
-- Returns 'Nothing' in case of parsing failure.
--
-- @since 0.3.0.0
parseSha256 :: Text -> Maybe Sha256
parseSha256 t
  | B.length s == 32, B.null rest = Just (Sha256 s)
  | otherwise                     = Nothing
  where
    (s, rest) = B16.decode $ T.encodeUtf8 t

-- | Export the 'Sha256' digest to a 32-byte 'B.ByteString'.
--
-- @since 0.3.0.0
sha256ToByteString :: Sha256 -> B.ByteString
sha256ToByteString (Sha256 bs) = bs

-- | Import the 'Sha256' digest from a 32-byte 'B.ByteString'.
--
-- Returns 'Nothing' if input 'B.ByteString' has incorrect length.
--
-- @since 0.3.0.0
sha256FromByteString :: B.ByteString -> Maybe Sha256
sha256FromByteString bs
  | B.length bs == 32  = Just (Sha256 bs)
  | otherwise          = Nothing

instance FromJSON Sha256 where
    parseJSON = withText "Sha256" (maybe (fail "Sha256") pure . parseSha256)

instance ToJSON Sha256 where
    toJSON = toJSON . dispSha256

instance Show Sha256 where
    show = show . dispSha256

----------------------------------------------------------------------------

-- | Extract directed 'UnitId' dependency graph edges from 'pjUnits'
--
-- This graph contains both, library and executable dependencies edges
planJsonIdGraph :: PlanJson -> Map UnitId (Set UnitId)
planJsonIdGraph PlanJson{..} = M.fromList [ (uId unit, planItemAllDeps unit)
                                          | unit <- M.elems pjUnits
                                          ]

-- | Extract 'UnitId' root nodes from dependency graph computed by 'planJsonIdGraph'
planJsonIdRoots :: PlanJson -> Set UnitId
planJsonIdRoots PlanJson{..} = M.keysSet pjUnits `S.difference` nonRoots
  where
    nonRoots :: Set UnitId
    nonRoots = mconcat $ M.elems $ planJsonIdGraph PlanJson{..}
