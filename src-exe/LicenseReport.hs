{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | SPDX-License-Identifier: GPL-2.0-or-later
--
-- Implements @cabal-plan license-report@ functionality
module LicenseReport
    ( generateLicenseReport
    ) where

#if defined(MIN_VERSION_Cabal)
import           Cabal.Plan
import qualified Codec.Archive.Tar                      as Tar
import qualified Codec.Archive.Tar.Entry                as Tar
import qualified Codec.Compression.GZip                 as GZip
import           Control.Monad.Compat                   (forM, forM_, guard, unless, when)
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.ByteString                        as BS
import           Data.Map                               (Map)
import           Data.List                              (nub)
import qualified Data.Map                               as Map
import           Data.Semigroup
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Data.Version                           as DV
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Pretty
import           Distribution.Utils.ShortText           (fromShortText)
import           System.Directory
import           System.FilePath
import           System.IO                              (stderr)
import           Text.ParserCombinators.ReadP
import           Prelude ()
import           Prelude.Compat

-- | Read tarball lazily (and possibly decompress)
readTarEntries :: FilePath -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case takeExtension idxtar of
            ".gz"  -> Tar.read . GZip.decompress <$> BSL.readFile idxtar
            ".tar" -> Tar.read                   <$> BSL.readFile idxtar
            ext    -> error ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

fp2pid :: FilePath -> Maybe PkgId
fp2pid fn0 = do
  [pns,pvs,rest] <- Just (splitDirectories fn0)
  guard (rest == pns <.> "cabal")
  pv <- parseVer pvs
  pure (PkgId (PkgName $ T.pack pns) pv)


parseVer :: String -> Maybe Ver
parseVer str = case reverse $ readP_to_S DV.parseVersion str of
  (ver, "") : _ | not (null (DV.versionBranch ver)), all (>= 0) (DV.versionBranch ver)
      -> Just (Ver $ DV.versionBranch ver)
  _   -> Nothing


readHackageIndex :: IO [(PkgId, BSL.ByteString)]
readHackageIndex = do
    -- TODO: expose package index configuration as CLI flag
    cabalPkgCacheDir <- getAppUserDataDirectory "cabal/packages/hackage.haskell.org"
    ents <- readTarEntries (cabalPkgCacheDir </> "01-index.tar")

    pure [ (maybe (error $ show n) id $ fp2pid n,bsl)
         | e@(Tar.Entry { Tar.entryContent = Tar.NormalFile bsl _ }) <- ents
         , let n = Tar.entryPath e
         , takeExtension n == ".cabal"
         ]

getLicenseFiles :: PkgId -> UnitId -> [FilePath] -> IO [BS.ByteString]
getLicenseFiles compilerId (UnitId uidt) fns = do
  storeDir <- getAppUserDataDirectory "cabal/store"
  let docDir = storeDir </> T.unpack (dispPkgId compilerId) </> T.unpack uidt </> "share" </> "doc"
  forM fns $ \fn -> BS.readFile (docDir </> fn)

{- WARNING: the code that follows will make you cry; a safety pig is provided below for your benefit.

                         _
 _._ _..._ .-',     _.._(`))
'-. `     '  /-._.-'    ',/
   )         \            '.
  / _    _    |             \
 |  a    a    /              |
 \   .-.                     ;
  '-('' ).-'       ,'       ;
     '-;           |      .'
        \           \    /
        | 7  .__  _.-\   \
        | |  |  ``/  /`  /
       /,_|  |   /,_/   /
          /,_/      '`-'

-}

-- TODO: emit report to Text or Text builder
generateLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> IO ()
generateLicenseReport mlicdir plan uid0 cn0 = do
    let pidsOfInterest = Set.fromList (map uPId (Map.elems $ pjUnits plan))

    indexDb <- Map.fromList . filter (flip Set.member pidsOfInterest . fst) <$> readHackageIndex

    let -- generally, units belonging to the same package as 'root'
        rootPkgUnits = [ u | u@(Unit { uPId = PkgId pn' _ }) <- Map.elems (pjUnits plan), pn' == pn0 ]
        rootPkgUnitIds = Set.fromList (map uId rootPkgUnits)

        -- the component of interest
        Just root@Unit { uPId = PkgId pn0 _ } = Map.lookup uid0 (pjUnits plan)

        fwdDeps = planJsonIdGraph' plan
        revDeps = invertMap fwdDeps

    let transUids = transDeps fwdDeps (uId root) Set.\\ rootPkgUnitIds

        indirectDeps = Set.fromList [ u | u <- Set.toList transUids, Set.null (Map.findWithDefault mempty u revDeps `Set.intersection` rootPkgUnitIds) ]

        directDeps = transUids Set.\\ indirectDeps


    let printInfo :: UnitId -> IO ()
        printInfo uid = do
          let Just u = Map.lookup uid (pjUnits plan)

              PkgId (PkgName pn) pv = uPId u
              isB = uType u == UnitTypeBuiltin
              url = "http://hackage.haskell.org/package/" <> dispPkgId (uPId u)

              -- special core libs whose reverse deps are too noisy
              baseLibs = ["base", "ghc-prim", "integer-gmp", "integer-simple", "rts"]

              usedBy = Set.fromList [ uPId (Map.findWithDefault undefined unit (pjUnits plan))
                                    | unit <- Set.toList (Map.findWithDefault mempty uid revDeps)
                                    , unit `Set.member` (directDeps <> indirectDeps)
                                    ]

          case BSL.toStrict <$> Map.lookup (uPId u) indexDb of
            Nothing
              | PkgId (PkgName "rts") _ <- uPId u -> pure ()
              | otherwise -> do
                  -- not found in index -- fail gracefully
                  T.hPutStrLn stderr ("WARNING: couldn't find metadata for " <> dispPkgId (uPId u))

                  T.putStrLn $ mconcat
                    [ if isB then "| **`" else "| `", pn, if isB then "`** | [`" else "` | [`", dispVer pv, "`](", url , ")", " | "
                    , " *MISSING* | *MISSING* | "
                    , if pn `elem` baseLibs then "*(core library)*"
                      else T.intercalate ", " [ T.singleton '`' <> (j :: T.Text) <> "`" | PkgId (z@(PkgName j)) _ <- Set.toList usedBy,  z /= pn0], " |"
                    ]

            Just x -> do
              gpd <- maybe (fail "parseGenericPackageDescriptionMaybe") pure $
                     parseGenericPackageDescriptionMaybe x

              let desc = escapeDesc $ fromShortText $ synopsis $ packageDescription gpd
                  lic  = license  $ packageDescription gpd
                  -- cr   = copyright $ packageDescription gpd
                  lfs  = licenseFiles $ packageDescription gpd


              let

                  licurl = case lfs of
                             [] -> url
                             (l:_)
                               | Just licdir <- mlicdir, uType u == UnitTypeGlobal -> T.pack (licdir </> T.unpack (dispPkgId (uPId u)) </> takeFileName l)
                               | otherwise              -> url <> "/src/" <> T.pack l

              T.putStrLn $ mconcat
                [ if isB then "| **`" else "| `", pn, if isB then "`** | [`" else "` | [`", dispVer pv, "`](", url , ")", " | "
                , "[`", T.pack (prettyShow lic), "`](", licurl , ")", " | "
                , T.pack desc, " | "
                , if pn `elem` baseLibs then "*(core library)*"
                  else T.intercalate ", " [ T.singleton '`' <> (j :: T.Text) <> "`" | PkgId (z@(PkgName j)) _ <- Set.toList usedBy,  z /= pn0], " |"
                ]

              -- print (pn, pv, prettyShow lic, cr, lfs, [ j | PkgId (PkgName j) _ <- Set.toList usedBy ])

              forM_ mlicdir $ \licdir -> do

                case uType u of
                  UnitTypeGlobal -> do
                    let lfs' = nub (map takeFileName lfs)

                    when (length lfs' /= length lfs) $ do
                      T.hPutStrLn stderr ("WARNING: Overlapping license filenames for " <> dispPkgId (uPId u))

                    crdat <- getLicenseFiles (pjCompilerId plan) uid lfs'

                    forM_ (zip lfs' crdat) $ \(fn,txt) -> do
                      let d = licdir </> T.unpack (dispPkgId (uPId u))
                      createDirectoryIfMissing True d
                      BS.writeFile (d </> fn) txt

                    -- forM_ crdat $ print
                    pure ()

                  -- TODO:
                  --   UnitTypeBuiltin
                  --   UnitTypeLocal
                  --   UnitTypeInplace

                  UnitTypeBuiltin -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (global/GHC bundled) not copied")
                  UnitTypeLocal   -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (project-local package) not copied")
                  UnitTypeInplace -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (project-inplace package) not copied")

                unless (length lfs == Set.size (Set.fromList lfs)) $
                  fail ("internal invariant broken for " <> show (uPId u))

          pure ()

    T.putStrLn "# Dependency License Report"
    T.putStrLn ""
    T.putStrLn ("Bold-faced **`package-name`**s denote standard libraries bundled with `" <> dispPkgId (pjCompilerId plan) <> "`.")
    T.putStrLn ""

    T.putStrLn ("## Direct dependencies of `" <> unPkgN pn0 <> ":" <> dispCompNameTarget pn0 cn0 <> "`")
    T.putStrLn ""
    T.putStrLn "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |"
    T.putStrLn "| --- | --- | --- | --- | --- |"
    forM_ directDeps $ printInfo
    T.putStrLn ""

    T.putStrLn "## Indirect transitive dependencies"
    T.putStrLn ""
    T.putStrLn "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |"
    T.putStrLn "| --- | --- | --- | --- | --- |"
    forM_ indirectDeps $ printInfo
    T.putStrLn ""

    pure ()

escapeDesc :: String -> String
escapeDesc []          = []
escapeDesc ('\n':rest) = ' ':escapeDesc rest
escapeDesc ('|':rest)  = '\\':'|':escapeDesc rest
escapeDesc (x:xs)      = x:escapeDesc xs

unPkgN :: PkgName -> T.Text
unPkgN (PkgName t) = t

planItemAllLibDeps :: Unit -> Set.Set UnitId
planItemAllLibDeps Unit{..} = mconcat [ ciLibDeps | (cn,CompInfo{..}) <- Map.toList uComps, wantC cn ]
  where
    wantC (CompNameSetup)   = False
    wantC (CompNameTest _)  = False
    wantC (CompNameBench _) = False
    wantC _                 = True

planJsonIdGraph':: PlanJson -> Map UnitId (Set UnitId)
planJsonIdGraph' PlanJson{..} = Map.fromList [ (uId unit, planItemAllLibDeps unit) | unit <- Map.elems pjUnits ]



invertMap :: Ord k => Map k (Set k) -> Map k (Set k)
invertMap m0 = Map.fromListWith mappend [ (v, Set.singleton k) | (k,vs) <- Map.toList m0, v <- Set.toList vs ]

transDeps :: Map UnitId (Set UnitId) -> UnitId -> Set UnitId
transDeps g n0 = go mempty [n0]
  where
    go :: Set UnitId -> [UnitId] -> Set UnitId
    go acc [] = acc
    go acc (n:ns)
      | Set.member n acc = go acc ns
      | otherwise = go (Set.insert n acc) (ns ++ Set.toList (Map.findWithDefault undefined n g))

#else

----------------------------------------------------------------------------
import           Cabal.Plan
import           System.Exit
import           System.IO

generateLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> IO ()
generateLicenseReport _ _ _ _ = do
  hPutStrLn stderr "ERROR: `cabal-plan license-report` sub-command not available! Please recompile/reinstall `cabal-plan` with the `license-report` Cabal flag activated."
  exitFailure

#endif
