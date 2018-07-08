{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Monad.Compat        (forM_, guard, unless, when)
import           Control.Monad.RWS.Strict    (RWS, evalRWS, gets, modify', tell)
import           Control.Monad.ST            (runST)
import           Data.Char                   (isAlphaNum)
import           Data.Foldable               (for_, toList)
import qualified Data.Graph                  as G
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, isJust, mapMaybe)
import           Data.Monoid                 (Any (..))
import           Data.Semigroup              (Semigroup (..))
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Builder      as LT
import qualified Data.Text.Lazy.IO           as LT
import qualified Data.Tree                   as Tr
import           Data.Tuple                  (swap)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Version
import           Options.Applicative
import           System.Console.ANSI
import           System.Directory            (getCurrentDirectory)
import           System.Exit                 (exitFailure)
import           System.IO                   (hPutStrLn, stderr)
import qualified Text.Parsec                 as P
import qualified Text.Parsec.String          as P
import qualified Topograph                   as TG

import           Cabal.Plan
import           LicenseReport               (generateLicenseReport)
import           Paths_cabal_plan            (version)

haveUnderlineSupport :: Bool
#if defined(UNDERLINE_SUPPORT)
haveUnderlineSupport = True
#else
haveUnderlineSupport = False
#endif

data GlobalOptions = GlobalOptions
    { buildDir        :: Maybe FilePath
    , optsShowBuiltin :: Bool
    , optsShowGlobal  :: Bool
    , cmd             :: Command
    }

data Command
    = InfoCommand
    | ShowCommand
    | FingerprintCommand
    | ListBinsCommand MatchCount [Pattern]
    | DotCommand Bool Bool [Highlight]
    | TopoCommand Bool
    | LicenseReport (Maybe FilePath) Pattern

-------------------------------------------------------------------------------
-- Pattern
-------------------------------------------------------------------------------

-- | patterns are @[[pkg:]kind;]cname@
data Pattern = Pattern (Maybe T.Text) (Maybe CompType) (Maybe T.Text)
    deriving (Show, Eq)

data CompType = CompTypeLib | CompTypeFLib | CompTypeExe | CompTypeTest | CompTypeBench | CompTypeSetup
    deriving (Show, Eq, Enum, Bounded)

parsePattern :: String -> Either String Pattern
parsePattern = either (Left . show) Right . P.runParser (patternP <* P.eof) () "<argument>"
  where
    patternP = do
        -- first we parse up to 3 tokens
        x <- tokenP
        y <- optional $ do
            _ <- P.char ':'
            y <- tokenP
            z <- optional $ P.char ':' >> tokenP
            return (y, z)
        -- then depending on how many tokens we got, we make a pattern
        case y of
            Nothing -> return $ Pattern Nothing Nothing x
            Just (y', Nothing) -> do
                t <- traverse toCompType x
                return $ Pattern Nothing t y'
            Just (y', Just z') -> do
                t <-  traverse toCompType y'
                return $ Pattern x t z'

    tokenP :: P.Parser (Maybe T.Text)
    tokenP =
        Nothing <$ P.string "*"
        <|> (Just . T.pack <$> some (P.satisfy (\c -> isAlphaNum c || c `elem` ("-_" :: String))) P.<?> "part of pattern")

    toCompType :: T.Text -> P.Parser CompType
    toCompType "bench" = return $ CompTypeBench
    toCompType "exe"   = return $ CompTypeExe
    toCompType "lib"   = return $ CompTypeLib
    toCompType "flib"  = return $ CompTypeFLib
    toCompType "setup" = return $ CompTypeSetup
    toCompType "test"  = return $ CompTypeTest
    toCompType t       = fail $ "Unknown component type: " ++ show t

patternCompleter :: Bool -> Completer
patternCompleter onlyWithExes = mkCompleter $ \pfx -> do
    plan <- getCurrentDirectory >>= findAndDecodePlanJson . ProjectRelativeToDir
    let tpfx  = T.pack pfx
        components = findComponents plan

    -- One scenario
    -- $ cabal-plan list-bin cab<TAB>
    -- $ cabal-plan list-bin cabal-plan<TAB>
    -- $ cabal-plan list-bin cabal-plan:exe:cabal-plan
    --
    -- Note: if this package had `tests` -suite, then we can
    -- $ cabal-plan list-bin te<TAB>
    -- $ cabal-plan list-bin tests<TAB>
    -- $ cabal-plan list-bin cabal-plan:test:tests
    --
    -- *BUT* at least zsh script have to be changed to complete from non-prefix.
    return $ map T.unpack $ firstNonEmpty
        -- 1. if tpfx matches component exacty, return full path
        [ single $ map fst $ filter ((tpfx ==) . snd) components

        -- 2. match component parts
        , uniques $ filter (T.isPrefixOf tpfx) $ map snd components

        -- otherwise match full paths
        , filter (T.isPrefixOf tpfx) $ map fst components
        ]
  where
    firstNonEmpty :: [[a]] -> [a]
    firstNonEmpty []         = []
    firstNonEmpty ([] : xss) = firstNonEmpty xss
    firstNonEmpty (xs : _)   = xs

    -- single
    single :: [a] -> [a]
    single xs@[_] = xs
    single _      = []

    -- somewhat like 'nub' but drop duplicate names. Doesn't preserve order
    uniques :: Ord a => [a] -> [a]
    uniques = M.keys . M.filter (== 1) . M.fromListWith (+) . map (\x -> (x, 1 :: Int))

    impl :: Bool -> Bool -> Bool
    impl False _ = True
    impl True  x = x

    -- returns (full, cname) pair
    findComponents :: PlanJson -> [(T.Text, T.Text)]
    findComponents plan = do
        (_, Unit{..}) <- M.toList $ pjUnits plan
        (cn, ci) <- M.toList $ uComps

        -- if onlyWithExes, component should have binFile
        guard (onlyWithExes `impl` isJust (ciBinFile ci))

        let PkgId pn@(PkgName pnT) _ = uPId
            g = case cn of
                CompNameLib -> pnT <> T.pack":lib:" <> pnT
                _           -> pnT <> T.pack":" <> dispCompName cn

        let cnT = extractCompName pn cn
        [ (g, cnT) ]

compNameType :: CompName -> CompType
compNameType CompNameLib        = CompTypeLib
compNameType (CompNameSubLib _) = CompTypeLib
compNameType (CompNameFLib _)   = CompTypeFLib
compNameType (CompNameExe _)    = CompTypeExe
compNameType (CompNameTest _)   = CompTypeTest
compNameType (CompNameBench _)  = CompTypeBench
compNameType CompNameSetup      = CompTypeSetup

checkPattern :: Pattern -> PkgName -> CompName -> Any
checkPattern (Pattern n k c) pn cn =
    Any $ nCheck && kCheck && cCheck
  where
    nCheck = case n of
        Nothing  -> True
        Just pn' -> pn == PkgName pn'

    kCheck = case k of
        Nothing -> True
        Just k' -> k' == compNameType cn
    cCheck = case c of
        Nothing -> True
        Just c' -> c' == extractCompName pn cn

extractCompName :: PkgName -> CompName -> T.Text
extractCompName (PkgName pn) CompNameLib         = pn
extractCompName (PkgName pn) CompNameSetup       = pn
extractCompName _            (CompNameSubLib cn) = cn
extractCompName _            (CompNameFLib cn)   = cn
extractCompName _            (CompNameExe cn)    = cn
extractCompName _            (CompNameTest cn)   = cn
extractCompName _            (CompNameBench cn)  = cn

-------------------------------------------------------------------------------
-- Highlight
-------------------------------------------------------------------------------

data Highlight
    = Path Pattern Pattern
    | Revdep Pattern
    deriving (Show, Eq)

highlightParser :: Parser Highlight
highlightParser = pathParser <|> revdepParser
  where
    pathParser = Path
        <$> option (eitherReader parsePattern)
            (long "path-from" <> metavar "PATTERN" <> help "Highlight dependency paths from ...")
        <*> option (eitherReader parsePattern)
            (long "path-to" <> metavar "PATTERN")

    revdepParser = Revdep
        <$> option (eitherReader parsePattern)
            (long "revdep" <> metavar "PATTERN" <> help "Highlight reverse dependencies")

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    GlobalOptions{..} <- execParser $ info (helper <*> optVersion <*> optParser) fullDesc
    (searchMethod, mProjRoot) <- case buildDir of
            Just dir -> pure (InBuildDir dir, Nothing)
            Nothing -> do
                cwd <- getCurrentDirectory
                root <- findProjectRoot cwd
                pure (ProjectRelativeToDir cwd, root)

    plan <- findAndDecodePlanJson searchMethod
    case cmd of
      InfoCommand -> doInfo mProjRoot plan
      ShowCommand -> mapM_ print mProjRoot >> print plan
      ListBinsCommand count pats -> do
          let bins = doListBin plan pats
          case (count, bins) of
              (MatchMany, _) -> for_ bins $ \(g, fn) ->
                    putStrLn (g ++ "  " ++ fn)
              (MatchOne, [(_,p)]) -> putStrLn p
              (MatchOne, []) -> do
                 hPutStrLn stderr "No matches found."
                 exitFailure
              (MatchOne, _) -> do
                 hPutStrLn stderr "Found more than one matching pattern:"
                 for_ bins $ \(p,_) -> hPutStrLn stderr $ "  " ++ p
                 exitFailure
      FingerprintCommand -> doFingerprint plan
      DotCommand tred tredWeights highlights -> doDot optsShowBuiltin optsShowGlobal plan tred tredWeights highlights
      TopoCommand rev -> doTopo optsShowBuiltin optsShowGlobal plan rev
      LicenseReport mfp pat -> doLicenseReport mfp pat
  where
    optVersion = infoOption ("cabal-plan " ++ showVersion version)
                            (long "version" <> help "output version information and exit")

    optParser = GlobalOptions
        <$> dirParser
        <*> showHide "builtin" "Show / hide packages in global (non-nix-style) package db"
        <*> showHide "global" "Show / hide packages in nix-store"
        <*> (cmdParser <|> defaultCommand)

    showHide n d =
        flag' True (long ("show-" ++ n) <> help d)
        <|> flag' False (long ("hide-" ++ n))
        <|> pure True

    dirParser = optional . strOption $ mconcat
        [ long "builddir", metavar "DIR"
        , help "Build directory to read plan.json from." ]

    subCommand name desc val = command name $ info val (progDesc desc)

    patternParser = argument (eitherReader parsePattern) . mconcat

    switchM = switch . mconcat

    cmdParser = subparser $ mconcat
        [ subCommand "info" "Info" $ pure InfoCommand
        , subCommand "show" "Show" $ pure ShowCommand
        , subCommand "list-bins" "List All Binaries" .
            listBinParser MatchMany . many $ patternParser
                [ metavar "PATTERNS...", help "Patterns to match.", completer $ patternCompleter True ]
        , subCommand "list-bin" "List Single Binary" .
            listBinParser MatchOne $ pure <$> patternParser
                [ metavar "PATTERN", help "Pattern to match.", completer $ patternCompleter True ]
        , subCommand "fingerprint" "Fingerprint" $ pure FingerprintCommand
        , subCommand "dot" "Dependency .dot" $ DotCommand
              <$> switchM
                  [ long "tred", help "Transitive reduction" ]
              <*> switchM
                  [ long "tred-weights", help "Adjust edge thickness during transitive reduction" ]
              <*> many highlightParser
              <**> helper
        , subCommand "topo" "Plan in a topological sort" $ TopoCommand
              <$> switchM
                  [ long "reverse", help "Reverse order" ]
              <**> helper
        , subCommand "license-report" "Generate license report for a component" $ LicenseReport
              <$> optional (strOption $ mconcat [ long "licensedir", metavar "DIR", help "Write per-package license documents to folder" ])
              <*> patternParser
                  [ metavar "PATTERN", help "Pattern to match.", completer $ patternCompleter False ]
              <**> helper
        ]

    defaultCommand = pure InfoCommand

-------------------------------------------------------------------------------
-- list-bin
-------------------------------------------------------------------------------

listBinParser
    :: MatchCount
    -> Parser [Pattern]
    -> Parser Command
listBinParser count pats
    = ListBinsCommand count <$> pats <**> helper
data MatchCount = MatchOne | MatchMany
    deriving (Show, Eq)

doListBin :: PlanJson -> [Pattern] -> [(String, FilePath)]
doListBin plan patterns = do
    (_, Unit{..}) <- M.toList $ pjUnits plan
    (cn, ci) <- M.toList $ uComps
    case ciBinFile ci of
        Nothing -> []
        Just fn -> do
            let PkgId pn@(PkgName pnT) _ = uPId
                g = case cn of
                    CompNameLib -> T.unpack (pnT <> T.pack":lib:" <> pnT)
                    _           -> T.unpack (pnT <> T.pack":" <> dispCompName cn)
            guard . getAny $ patternChecker pn cn
            [(g, fn)]
  where
    patternChecker :: PkgName -> CompName -> Any
    patternChecker = case patterns of
        [] -> \_ _ -> Any True
        _  -> mconcat $ map checkPattern patterns

-------------------------------------------------------------------------------
-- fingerprint
-------------------------------------------------------------------------------

doFingerprint :: PlanJson -> IO ()
doFingerprint plan = do
    let pids = M.fromList [ (uPId u, u) | (_,u) <- M.toList (pjUnits plan) ]

    for_ (M.toList pids) $ \(_,Unit{..}) -> do
        let h = maybe "________________________________________________________________"
                      dispSha256 $ uSha256
        case uType of
          UnitTypeBuiltin -> T.putStrLn (h <> " B " <> dispPkgId uPId)
          UnitTypeGlobal  -> T.putStrLn (h <> " G " <> dispPkgId uPId)
          UnitTypeLocal   -> T.putStrLn (h <> " L " <> dispPkgId uPId)
          UnitTypeInplace -> T.putStrLn (h <> " I " <> dispPkgId uPId)

-------------------------------------------------------------------------------
-- info
-------------------------------------------------------------------------------

doInfo :: Maybe FilePath -> PlanJson -> IO ()
doInfo mProjbase plan = do
    forM_ mProjbase $ \projbase ->
        putStrLn ("using '" ++ projbase ++ "' as project root")
    putStrLn ""
    putStrLn "Tree"
    putStrLn "~~~~"
    putStrLn ""
    LT.putStrLn (dumpPlanJson plan)

    -- print (findCycles (planJsonIdGrap v))

    putStrLn ""
    putStrLn "Top-sorted"
    putStrLn "~~~~~~~~~~"
    putStrLn ""

    let xs = toposort (planJsonIdGraph plan)
    for_ xs print

    putStrLn ""
    putStrLn "Direct deps"
    putStrLn "~~~~~~~~~~~"
    putStrLn ""

    let locals = [ Unit{..} | Unit{..} <- M.elems pm, uType == UnitTypeLocal ]
        pm = pjUnits plan

    for_ locals $ \pitem -> do
        print (uPId pitem)
        for_ (M.toList $ uComps pitem) $ \(ct,ci) -> do
            print ct
            for_ (S.toList $ ciLibDeps ci) $ \dep -> do
                let Just dep' = M.lookup dep pm
                    pid = uPId dep'
                putStrLn ("  " ++ T.unpack (dispPkgId pid))
            putStrLn ""

    return ()

-------------------------------------------------------------------------------
-- Dot
-------------------------------------------------------------------------------

-- | vertex of dot graph.
--
-- if @'Maybe' 'CompName'@ is Nothing, this is legacy, multi-component unit.
data DotUnitId = DU UnitId (Maybe CompName)
    deriving (Eq, Ord, Show)

planJsonDotUnitGraph :: PlanJson -> Map DotUnitId (Set DotUnitId)
planJsonDotUnitGraph plan = M.fromList $ do
    unit <- M.elems units
    let mkDU = DU (uId unit)
    let mkDeps cname ci = (mkDU (Just cname), deps ci)
    case M.toList (uComps unit) of
        [(cname, ci)] ->
            [ mkDeps cname ci ]
        cs            ->
            [ (mkDU Nothing, S.fromList $ map (mkDU . Just . fst) cs) ]
            ++ map (uncurry mkDeps) cs
  where
    units = pjUnits plan

    unitToDot :: Unit -> DotUnitId
    unitToDot unit = DU (uId unit) $ case M.toList (uComps unit) of
        [(cname, _)] -> Just cname
        _            -> Nothing

    unitIdToDot :: UnitId -> Maybe DotUnitId
    unitIdToDot i = unitToDot <$> M.lookup i units

    deps :: CompInfo -> Set DotUnitId
    deps CompInfo{..} =
        S.fromList $ mapMaybe unitIdToDot $ S.toList $ ciLibDeps <> ciExeDeps

-- | Tree which counts paths under it.
data Tr a = No !Int a [Tr a]
  deriving (Show)

trPaths :: Tr a -> Int
trPaths (No n _ _) = n

-- | Create 'Tr' maintaining the invariant
mkNo :: a -> [Tr a] -> Tr a
mkNo x [] = No 1 x []
mkNo x xs = No (sum $ map trPaths xs) x xs

trFromTree :: Tr.Tree a -> Tr a
trFromTree (Tr.Node i is) = mkNo i (map trFromTree is)

trPairs :: Tr a -> [(Int,a,a)]
trPairs (No _ i js) =
    [ (n, i, j) | No n j _ <- js ] ++ concatMap trPairs js

doDot :: Bool -> Bool -> PlanJson -> Bool -> Bool -> [Highlight] -> IO ()
doDot showBuiltin showGlobal plan tred tredWeights highlights = either loopGraph id $ TG.runG am $ \g' -> do
    let g = if tred then TG.reduction g' else g'

    -- Highlights
    let paths :: [(DotUnitId, DotUnitId)]
        paths = flip concatMap highlights $ \h -> case h of
            Path a b ->
                [ (x, y)
                | x <- filter (getAny . checkPatternDotUnit a) $ toList dotUnits
                , y <- filter (getAny . checkPatternDotUnit b) $ toList dotUnits
                ]
            Revdep _ -> []

    let paths' :: [(DotUnitId, DotUnitId)]
        paths' = flip concatMap paths $ \(a, b) -> fromMaybe [] $ do
            i <- TG.gToVertex g a
            j <- TG.gToVertex g b
            pure $ concatMap TG.pairs $ (fmap . fmap) (TG.gFromVertex g) (TG.allPaths g i j)

    let revdeps :: [DotUnitId]
        revdeps = flip concatMap highlights $ \h -> case h of
            Path _ _ -> []
            Revdep a -> filter (getAny . checkPatternDotUnit a) $ toList dotUnits

    let tg = TG.transpose g

    let revdeps' :: [(DotUnitId, DotUnitId)]
        revdeps' = flip concatMap revdeps $ \a -> fromMaybe [] $ do
            i <- TG.gToVertex tg a
            pure $ map swap $ TG.treePairs $ fmap (TG.gFromVertex tg) (TG.dfsTree tg i)

    let redVertices :: Set DotUnitId
        redVertices = foldMap (\(a,b) -> S.fromList [a,b]) $ paths' ++ revdeps'

    let redEdges :: Set (DotUnitId, DotUnitId)
        redEdges = S.fromList $ paths' ++ revdeps'

    -- Edge weights
    let weights' :: U.Vector Double
        weights' = runST $ do
            let orig = TG.edgesSet g'
                redu = TG.edgesSet g
                len  = TG.gVerticeCount g
            v <- MU.replicate (len * len) (0 :: Double)

            -- for each edge (i, j) in original graph, but not in the reduction
            for_ (S.difference orig redu) $ \(i, j) -> do
                -- calculate all paths from i to j, in the reduction
                for_ (fmap trFromTree $ TG.allPathsTree g i j) $ \ps -> do
                    -- divide weight across paths
                    let r  = 1 / fromIntegral (trPaths ps)

                    -- and add that weight to every edge on each path
                    for_ (trPairs ps) $ \(k, a, b) ->
                        MU.modify v
                            (\n -> n + fromIntegral k * r)
                            (TG.gToInt g b + TG.gToInt g a * len)

            U.freeze v

    let weights :: Map (DotUnitId, DotUnitId) Double
        weights =
            if tred && tredWeights
            then M.fromList
                [ ((a, b), w + 1)
                | ((i, j), w) <- zip ((,) <$> TG.gVertices g <*> TG.gVertices g) (U.toList weights')
                , w > 0
                , let a = TG.gFromVertex g i
                , let b = TG.gFromVertex g j
                ]
            else M.empty

    -- Beging outputting

    putStrLn "digraph plan {"
    putStrLn "overlap = false;"
    putStrLn "rankdir=LR;"
    putStrLn "node [penwidth=2];"

    -- vertices
    for_ (TG.gVertices g) $ \i -> vertex redVertices (TG.gFromVertex g i)

    -- edges
    for_ (TG.gVertices g) $ \i -> for_ (TG.gEdges g i) $ \j ->
        edge weights redEdges (TG.gFromVertex g i) (TG.gFromVertex g j)

    putStrLn "}"
  where
    loopGraph [] = putStrLn "digraph plan {}"
    loopGraph (u : us) = do
        putStrLn "digraph plan {"
        for_ (zip (u : us) (us ++ [u])) $ \(unitA, unitB) ->
            T.putStrLn $ mconcat
                [ "\""
                , dispDotUnit unitA
                , "\""
                , " -> "
                , "\""
                , dispDotUnit unitB
                , "\""
                ]
        putStrLn "}"

    am = planJsonDotUnitGraph plan

    dotUnits :: Set DotUnitId
    dotUnits = S.fromList $ M.keys am

    units :: Map UnitId Unit
    units = pjUnits plan

    duShape :: DotUnitId -> T.Text
    duShape (DU unitId _) = case M.lookup unitId units of
        Nothing   -> "oval"
        Just unit -> case uType unit of
            UnitTypeBuiltin -> "octagon"
            UnitTypeGlobal  -> "box"
            UnitTypeInplace -> "box"
            UnitTypeLocal   -> "box,style=rounded"

    duShow :: DotUnitId -> Bool
    duShow (DU unitId _) = case M.lookup unitId units of
        Nothing -> False
        Just unit -> case uType unit of
            UnitTypeBuiltin -> showBuiltin
            UnitTypeGlobal  -> showGlobal
            UnitTypeLocal   -> True
            UnitTypeInplace -> True

    vertex :: Set DotUnitId -> DotUnitId -> IO ()
    vertex redVertices du = when (duShow du) $ T.putStrLn $ mconcat
        [ "\""
        , dispDotUnit du
        , "\""
        -- shape
        , " [shape="
        , duShape du
        -- color
        , ",color="
        , color
        , "];"
        ]
      where
        color | S.member du redVertices = "red"
              | otherwise               = borderColor du

    borderColor :: DotUnitId -> T.Text
    borderColor (DU _ Nothing)           = "darkviolet"
    borderColor (DU unitId (Just cname)) = case cname of
        CompNameLib         -> case M.lookup unitId units of
            Nothing   -> "black"
            Just unit -> case uType unit of
                UnitTypeLocal   -> "blue"
                UnitTypeInplace -> "blue"
                _               -> "black"
        (CompNameSubLib _)  -> "gray"
        (CompNameFLib _)    -> "darkred"
        (CompNameExe _)     -> "brown"
        (CompNameBench _)   -> "darkorange"
        (CompNameTest _)    -> "darkgreen"
        CompNameSetup       -> "gold"

    edge
        :: Map (DotUnitId, DotUnitId) Double
        -> Set (DotUnitId, DotUnitId)
        -> DotUnitId -> DotUnitId -> IO ()
    edge weights redEdges duA duB = when (duShow duA) $ when (duShow duB) $
        T.putStrLn $ mconcat
            [ "\""
            , dispDotUnit duA
            , "\""
            , " -> "
            , "\""
            , dispDotUnit duB
            , "\" [color="
            , color
            , ",penwidth="
            , T.pack $ show $ logBase 4 w + 1
            , ",weight="
            , T.pack $ show $ logBase 4 w + 1
            , "];"
            ]
      where
        idPair = (duA, duB)

        color | S.member idPair redEdges = "red"
              | otherwise                     = borderColor duA

        w = fromMaybe 1 $ M.lookup idPair weights

    checkPatternDotUnit :: Pattern -> DotUnitId -> Any
    checkPatternDotUnit p (DU unitId mcname) = case M.lookup unitId units of
        Nothing   -> Any False
        Just unit -> case mcname of
            Just cname -> checkPattern p pname cname
            Nothing    -> foldMap (checkPattern p pname) (M.keys (uComps unit))
          where
            PkgId pname _ = uPId unit

    dispDotUnit :: DotUnitId -> T.Text
    dispDotUnit (DU unitId mcname) = case M.lookup unitId units of
        Nothing   -> "?"
        Just unit -> dispPkgId (uPId unit) <> maybe ":*" dispCompName' mcname

    dispCompName' :: CompName -> T.Text
    dispCompName' CompNameLib = ""
    dispCompName' cname       = ":" <> dispCompName cname

-------------------------------------------------------------------------------
-- license-report
-------------------------------------------------------------------------------

doLicenseReport :: Maybe FilePath -> Pattern -> IO ()
doLicenseReport mlicdir pat = do
    plan <- getCurrentDirectory >>= findAndDecodePlanJson . ProjectRelativeToDir

    case findUnit plan of
      [] -> do
        hPutStrLn stderr "No matches found."
        exitFailure

      lst@(_:_:_) -> do
        hPutStrLn stderr "Multiple matching components found:"
        forM_ lst $ \(pat', uid, cn) -> do
          hPutStrLn stderr ("- " ++ T.unpack pat' ++ "   " ++ show (uid, cn))
        exitFailure

      [(_,uid,cn)] -> generateLicenseReport mlicdir plan uid cn

  where
    findUnit plan = do
        (_, Unit{..}) <- M.toList $ pjUnits plan
        (cn, _) <- M.toList $ uComps

        let PkgId pn@(PkgName pnT) _ = uPId
            g = case cn of
                CompNameLib -> pnT <> T.pack":lib:" <> pnT
                _           -> pnT <> T.pack":" <> dispCompName cn

        guard (getAny $ checkPattern pat pn cn)

        pure (g, uId, cn)


-------------------------------------------------------------------------------
-- topo
-------------------------------------------------------------------------------

doTopo :: Bool -> Bool -> PlanJson -> Bool -> IO ()
doTopo showBuiltin showGlobal plan rev = do
    let units = pjUnits plan

    let topo = TG.runG (planJsonIdGraph plan) $ \TG.G {..} ->
            map gFromVertex gVertices

    let showUnit unit = case uType unit of
          UnitTypeBuiltin -> showBuiltin
          UnitTypeGlobal  -> showGlobal
          UnitTypeLocal   -> True
          UnitTypeInplace -> True

    let rev' = if rev then reverse else id

    for_ topo $ \topo' -> for_ (rev' topo') $ \unitId ->
        for_ (M.lookup unitId units) $ \unit ->
            when (showUnit unit) $ do
                let colour = case uType unit of
                        UnitTypeBuiltin -> Blue
                        UnitTypeGlobal  -> White
                        UnitTypeLocal   -> Green
                        UnitTypeInplace -> Red
                let components = case M.keys (uComps unit) of
                        [] -> ""
                        [CompNameLib] -> ""
                        names -> " " <> T.intercalate " " (map dispCompName names)
                putStrLn $
                    colorify colour (T.unpack $ dispPkgId $ uPId unit)
                    ++ T.unpack components

----------------------------------------------------------------------------

dumpPlanJson :: PlanJson -> LT.Text
dumpPlanJson (PlanJson { pjUnits = pm }) = LT.toLazyText out
  where
    ((),out) = evalRWS (mapM_ (go2 []) (S.toList roots)) () mempty

    id2pid :: Map UnitId PkgId
    id2pid = M.fromList [ (uId, uPId) | Unit{..} <- M.elems pm ]

    lupPid uid = M.findWithDefault undefined uid id2pid

    go2 :: [(CompName,Bool)] -> UnitId -> (RWS () LT.Builder (Set UnitId)) ()
    go2 lvl pid = do
        pidSeen <- gets (S.member pid)

        let pid_label = if preExists then (prettyId pid) else colorify_ White (prettyId pid)

        if not pidSeen
         then do
            tell $ LT.fromString (linepfx ++ pid_label ++ "\n")
            showDeps
         else do
            tell $ LT.fromString (linepfx ++ pid_label ++ ccol CompNameLib " ┄┄\n")
            -- tell $ LT.fromString (linepfx' ++ " └┄\n")

        modify' (S.insert pid)

        return ()
      where
        Just x' = M.lookup pid pm

        preExists = uType x' == UnitTypeBuiltin

        showDeps = for_ (M.toList $ uComps x') $ \(ct,deps) -> do
            unless (ct == CompNameLib) $
                tell (LT.fromString $ linepfx' ++ " " ++ prettyCompTy (lupPid pid) ct ++ "\n")
            for_ (lastAnn $ S.toList (ciLibDeps deps)) $ \(l,y) -> do
                go2 (lvl ++ [(ct, not l)]) y


        linepfx = case unsnoc lvl of
                         Nothing -> ""
                         Just (xs,(zt,z)) -> concat [ if x then ccol xt " │ " else "   " | (xt,x) <- xs ]
                                        ++ (ccol zt $ if z then " ├─ " else " └─ ")

        linepfx' = concat [ if x then  " │ " else "   " | (_,x) <- lvl ]

    roots :: Set UnitId
    roots = M.keysSet pm `S.difference` leafs
      where
        leafs = mconcat $ concatMap (map (ciLibDeps . snd) . M.toList . uComps) (M.elems pm)

    prettyId :: UnitId -> String
    prettyId = prettyPid . lupPid
    prettyPid = T.unpack . dispPkgId

    prettyCompTy :: PkgId -> CompName -> String
    prettyCompTy _pid c@CompNameLib       = ccol c "[lib]"
    prettyCompTy _pid c@CompNameSetup     = ccol c "[setup]"
    prettyCompTy pid c@(CompNameExe n)    = ccol c $ "[" ++ prettyPid pid ++ ":exe:"   ++ show n ++ "]"
    prettyCompTy pid c@(CompNameTest n)   = ccol c $ "[" ++ prettyPid pid ++ ":test:"  ++ show n ++ "]"
    prettyCompTy pid c@(CompNameBench n)  = ccol c $ "[" ++ prettyPid pid ++ ":bench:" ++ show n ++ "]"
    prettyCompTy pid c@(CompNameSubLib n) = ccol c $ "[" ++ prettyPid pid ++ ":lib:" ++ show n ++ "]"
    prettyCompTy pid c@(CompNameFLib n)   = ccol c $ "[" ++ prettyPid pid ++ ":flib:" ++ show n ++ "]"

    ccol CompNameLib        = colorify White
    ccol (CompNameExe _)    = colorify Green
    ccol CompNameSetup      = colorify Red
    ccol (CompNameTest _)   = colorify Yellow
    ccol (CompNameBench _)  = colorify Cyan
    ccol (CompNameSubLib _) = colorify Blue
    ccol (CompNameFLib _)   = colorify Magenta

colorify :: Color -> String -> String
colorify col s = setSGRCode [SetColor Foreground Vivid col] ++ s ++ setSGRCode [Reset]

colorify_ :: Color -> String -> String
colorify_ col s
  | haveUnderlineSupport = setSGRCode [SetUnderlining SingleUnderline, SetColor Foreground Vivid col] ++ s ++ setSGRCode [Reset]
  | otherwise            = colorify col s

lastAnn :: [x] -> [(Bool,x)]
lastAnn = reverse . firstAnn . reverse

firstAnn :: [x] -> [(Bool,x)]
firstAnn []     = []
firstAnn (x:xs) = (True,x) : map ((,) False) xs

unsnoc :: [x] -> Maybe ([x],x)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

toposort :: Ord a => Map a (Set a) -> [a]
toposort m = reverse . map f . G.topSort $ g
  where
    (g, f) = graphFromMap m

graphFromMap :: Ord a => Map a (Set a) -> (G.Graph, G.Vertex -> a)
graphFromMap m = (g, v2k')
  where
    v2k' v = case v2k v of ((), k, _) -> k

    (g, v2k, _) = G.graphFromEdges [ ((), k, S.toList v)
                                   | (k,v) <- M.toList m ]
