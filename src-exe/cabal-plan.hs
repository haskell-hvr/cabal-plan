{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | SPDX-License-Identifier: GPL-2.0-or-later
module Main where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Monad.Compat        (forM_, guard, unless, when, ap)
import           Control.Monad.State.Strict  (StateT, modify', evalStateT, gets)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.ST            (runST)
import           Data.Align                  (align)
import           Data.Char                   (isAlphaNum)
import           Data.Foldable               (for_, toList)
import qualified Data.Graph                  as G
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe, isJust, mapMaybe, catMaybes)
import           Data.Monoid                 (Any (..), Endo (..))
import           Data.Semigroup              (Semigroup (..))
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.String                 (IsString (..))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Data.These                  (These (..))
import qualified Data.Tree                   as Tr
import           Data.Tuple                  (swap)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Version
import           Optics.Indexed.Core         (ifor_)
import           Options.Applicative
import           System.Console.ANSI
import           System.Directory            (getCurrentDirectory)
import           System.Exit                 (exitFailure)
import           System.IO                   (hPutStrLn, stderr, stdout)
import qualified Text.Parsec                 as P
import qualified Text.Parsec.String          as P
import qualified Topograph                   as TG
import Flag

import           Cabal.Plan
import           LicenseReport               (generateLicenseReport)
import           Paths_cabal_plan            (version)

haveUnderlineSupport :: Bool
#if defined(UNDERLINE_SUPPORT)
haveUnderlineSupport = True
#else
haveUnderlineSupport = False
#endif

data ShowBuiltin = ShowBuiltin
data ShowGlobal  = ShowGlobal
data ShowCabSha  = ShowCabSha
data DotTred     = DotTred
data DotTredWght = DotTredWght
data TopoReverse = TopoReverse
data ShowFlags   = ShowFlags

instance HasDefault 'True  ShowBuiltin
instance HasDefault 'True  ShowGlobal
instance HasDefault 'False ShowCabSha
instance HasDefault 'False DotTred
instance HasDefault 'False DotTredWght
instance HasDefault 'False TopoReverse
instance HasDefault 'False ShowFlags

data GlobalOptions = GlobalOptions
    { optsShowBuiltin :: Flag ShowBuiltin
    , optsShowGlobal  :: Flag ShowGlobal
    , optsUseColors   :: UseColors
    , cmd             :: Command
    }

data Command
    = InfoCommand        (Maybe SearchPlanJson)
    | ShowCommand        (Maybe SearchPlanJson)
    | TredCommand        (Maybe SearchPlanJson)
    | FingerprintCommand (Maybe SearchPlanJson) (Flag ShowCabSha)
    | ListBinsCommand    (Maybe SearchPlanJson) MatchCount [Pattern]
    | DotCommand         (Maybe SearchPlanJson) (Flag DotTred) (Flag DotTredWght) [Highlight]
    | TopoCommand        (Maybe SearchPlanJson) (Flag TopoReverse) (Flag ShowFlags)
    | LicenseReport      (Maybe FilePath) Pattern
    | DiffCommand        SearchPlanJson SearchPlanJson

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
            g = pnT <> T.pack":" <> dispCompNameTarget pn cn

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
    let prefs' = prefs $ subparserInline
    GlobalOptions{..} <- customExecParser prefs' $ info (helper <*> optVersion <*> optParser) fullDesc

    case cmd of
      InfoCommand s -> do
          (mProjRoot, plan) <- findPlan s
          doInfo optsUseColors mProjRoot plan
      ShowCommand s -> do
          (mProjRoot, plan) <- findPlan s
          mapM_ print mProjRoot
          print plan
      TredCommand s -> do
          (_, plan) <- findPlan s
          doTred optsUseColors plan
      DiffCommand old new -> do
          (_, oldPlan) <- findPlan (Just old)
          (_, newPlan) <- findPlan (Just new)
          doDiff optsUseColors oldPlan newPlan
      ListBinsCommand s count pats -> do
          (_, plan) <- findPlan s
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
      FingerprintCommand s showCabSha -> do
          (_, plan) <- findPlan s
          doFingerprint plan showCabSha
      DotCommand s tred tredWeights highlights -> do
          (_, plan) <- findPlan s
          doDot optsShowBuiltin optsShowGlobal plan tred tredWeights highlights
      TopoCommand s rev showFlags -> do
          (_, plan) <- findPlan s
          doTopo optsUseColors optsShowBuiltin optsShowGlobal plan rev showFlags
      LicenseReport mfp pat -> doLicenseReport mfp pat
  where
    findPlan search = do
        cwd <- getCurrentDirectory
        (searchMethod, mProjRoot) <- case search of
            Just searchMethod -> pure (searchMethod, Nothing)
            Nothing -> do
                root <- findProjectRoot cwd
                pure (ProjectRelativeToDir cwd, root)
        plan <- findAndDecodePlanJson searchMethod
        return (mProjRoot, plan)

    optVersion = infoOption ("cabal-plan " ++ showVersion version)
                            (long "version" <> help "output version information and exit")

    optParser = GlobalOptions
        <$> showHide ShowBuiltin "builtin" "Show / hide packages in global (non-nix-style) package db"
        <*> showHide ShowGlobal  "global"  "Show / hide packages in nix-store"
        <*> useColorsParser
        <*> (cmdParser <|> defaultCommand)



    useColorsParser :: Parser UseColors
    useColorsParser = option (eitherReader parseColor) $ mconcat
        [ long "color", metavar "always|never|auto"
        , help "Color output"
        , value ColorsAuto
        , showDefault
        , completer $ listCompleter ["always","never","auto"]
        ]

    parseColor :: String -> Either String UseColors
    parseColor "always" = Right ColorsAlways
    parseColor "never"  = Right ColorsNever
    parseColor "auto"   = Right ColorsAuto
    parseColor s        = Left $ "Use always, never or auto; not " ++ s

    subCommand name desc val = command name $ info val $ progDesc desc

    patternParser = argument (eitherReader parsePattern) . mconcat

    cmdParser = subparser $ mconcat
        [ subCommand "info" "Info" $ InfoCommand
            <$> planParser
        , subCommand "show" "Show" $ ShowCommand
            <$> planParser
        , subCommand "tred" "Transitive reduction" $ TredCommand
            <$> planParser
        , subCommand "diff" "Compare two plans" $ DiffCommand
            <$> planParser'
            <*> planParser'
        , subCommand "list-bins" "List All Binaries" .
            listBinParser MatchMany . many $ patternParser
                [ metavar "PATTERNS...", help "Patterns to match.", completer $ patternCompleter True ]
        , subCommand "list-bin" "List Single Binary" .
            listBinParser MatchOne $ pure <$> patternParser
                [ metavar "PATTERN", help "Pattern to match.", completer $ patternCompleter True ]
        , subCommand "fingerprint" "Print dependency hash fingerprint" $ FingerprintCommand
            <$> planParser
            <*> switchM ShowCabSha "show-cabal-sha256" ""
            <**> helper
        , subCommand "dot" "Dependency .dot" $ DotCommand
            <$> planParser
            <*> switchM DotTred     "tred"         "Transitive reduction"
            <*> switchM DotTredWght "tred-weights" "Adjust edge thickness during transitive reduction"
            <*> many highlightParser
            <**> helper
        , subCommand "topo" "Plan in a topological sort" $ TopoCommand
            <$> planParser
            <*> switchM TopoReverse "reverse"    "Reverse order"
            <*> switchM ShowFlags   "show-flags" "Show flag assignments"
            <**> helper
        , subCommand "license-report" "Generate license report for a component" $ LicenseReport
            <$> optional (strOption $ mconcat [ long "licensedir", metavar "DIR", help "Write per-package license documents to folder" ])
            <*> patternParser
                [ metavar "PATTERN", help "Pattern to match.", completer $ patternCompleter False ]
            <**> helper
        ]

    defaultCommand = pure (InfoCommand Nothing)

-------------------------------------------------------------------------------
-- Plan parser
-------------------------------------------------------------------------------

planParser :: Parser (Maybe SearchPlanJson)
planParser = optional planParser'

planParser' :: Parser SearchPlanJson
planParser' = InBuildDir <$> dirParser
        <|> ExactPath <$> planJsonParser
        <|> ProjectRelativeToDir <$> projectRootParser
  where
    dirParser = strOption $ mconcat
        [ long "builddir", metavar "DIR"
        , help "Build directory to read plan.json from."
        , completer (bashCompleter "directory")
        ]

    planJsonParser = strOption $ mconcat
        [ long "plan-json", metavar "PATH"
        , help "Exact location of plan.json."
        , completer (bashCompleter "file")
        ]

    projectRootParser = strOption $ mconcat
        [ long "relative", metavar "DIR"
        , help "Find the project root relative to specified directory."
        , completer (bashCompleter "directory")
        ]

-------------------------------------------------------------------------------
-- list-bin
-------------------------------------------------------------------------------

listBinParser
    :: MatchCount
    -> Parser [Pattern]
    -> Parser Command
listBinParser count pats =
    ListBinsCommand <$> planParser <*> pure count <*> pats <**> helper

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
                g = T.unpack pnT ++ ":" ++ T.unpack (dispCompNameTarget pn cn)
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

doFingerprint :: PlanJson -> Flag ShowCabSha -> IO ()
doFingerprint plan showCabSha = do
    let pids = M.fromList [ (uPId u, u) | (_,u) <- M.toList (pjUnits plan) ]

    for_ (M.toList pids) $ \(_,Unit{..}) -> do
        let h1 = maybe "________________________________________________________________"
                       dispSha256 $ uSha256
        let h2 = maybe "________________________________________________________________"
                       dispSha256 $ uCabalSha256

        let ty = case uType of
                   UnitTypeBuiltin -> "B"
                   UnitTypeGlobal  -> "G"
                   UnitTypeLocal   -> "L"
                   UnitTypeInplace -> "I"

        T.putStrLn (T.unwords $ if fromFlag ShowCabSha showCabSha then [ h1, h2,  ty, dispPkgId uPId ] else  [ h1, ty, dispPkgId uPId ])

-------------------------------------------------------------------------------
-- info
-------------------------------------------------------------------------------

doInfo :: UseColors -> Maybe FilePath -> PlanJson -> IO ()
doInfo useColors mProjbase plan = do
    forM_ mProjbase $ \projbase ->
        putStrLn ("using '" ++ projbase ++ "' as project root")
    putStrLn ""
    putStrLn "Tree"
    putStrLn "~~~~"
    putStrLn ""
    runCWriterIO useColors (dumpPlanJson plan)

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
-- tred - Transitive reduction
-------------------------------------------------------------------------------

doTred :: UseColors -> PlanJson -> IO ()
doTred useColors plan = runCWriterIO useColors (dumpTred plan)

dumpTred :: PlanJson -> CWriter ()
dumpTred plan = case fst <$> reductionClosureAM plan of
    Left  xs -> loopGraph xs
    Right am -> do
        let nonRoots :: Set DotUnitId
            nonRoots = mconcat $ M.elems am

            roots :: Set DotUnitId
            roots = M.keysSet am `S.difference` nonRoots

        evalStateT (mapM_ (go1 am) roots) S.empty
  where
    pm = pjUnits plan

    directDepsOfLocalPackages :: Set UnitId
    directDepsOfLocalPackages = S.fromList
        [ depUid
        | u <- M.elems pm
        , uType u == UnitTypeLocal
        , ci <- M.elems (uComps u)
        , depUid <- S.toList (ciLibDeps ci)
        ]

    loopGraph :: [DotUnitId] -> CWriter ()
    loopGraph xs = do
        putCTextLn $ colorifyStr Red $ "panic: Found a loop"
        mapM_ (putCTextLn . fromString . show) xs

    go1 :: Map DotUnitId (Set DotUnitId)
        -> DotUnitId
        -> StateT (Set DotUnitId) CWriter ()
    go1 am = go2 [] where
        ccol :: Maybe CompName -> String -> CText
        ccol Nothing     = colorifyStr White
        ccol (Just comp) = ccol' comp

        ccol' CompNameLib        = colorifyStr White
        ccol' (CompNameExe _)    = colorifyStr Green
        ccol' CompNameSetup      = colorifyStr Red
        ccol' (CompNameTest _)   = colorifyStr Yellow
        ccol' (CompNameBench _)  = colorifyStr Cyan
        ccol' (CompNameSubLib _) = colorifyStr Blue
        ccol' (CompNameFLib _)   = colorifyStr Magenta

        go2 :: [(Maybe CompName, Bool)]
            -> DotUnitId
            -> StateT (Set DotUnitId) CWriter ()
        go2 lvl duid@(DU uid comp) = do
            let unit = M.findWithDefault (error "non-existing UnitId") uid pm
            let deps = M.findWithDefault S.empty duid am
            let pid = uPId unit

            let emphasise' | uType unit == UnitTypeLocal             = underline
                           | uid `S.member` directDepsOfLocalPackages = emphasise
                           | otherwise                               = id

            seen <- gets (S.member duid)
            modify' (S.insert duid)

            let pid_label = emphasise' $ ccol comp (prettyCompTy pid comp)

            if seen
            then putCTextLn $ linepfx lvl <> pid_label <> " ┄┄"
            else do
                putCTextLn $ linepfx lvl <> pid_label

                for_ (lastAnn $ S.toList deps) $ \(l, depDuid) ->
                    go2 (lvl ++ [(comp, not l)]) depDuid

        linepfx :: [(Maybe CompName, Bool)] -> CText
        linepfx lvl = case unsnoc lvl of
           Nothing -> ""
           Just (xs,(zt,z)) -> mconcat [ if x then ccol xt " │ " else "   " | (xt,x) <- xs ]
                               <> (ccol zt $ if z then " ├─ " else " └─ ")

        prettyPid = T.unpack . dispPkgId

        prettyCompTy :: PkgId -> Maybe CompName -> String
        prettyCompTy pid Nothing  = "[" ++ prettyPid pid ++ ":all]"
        prettyCompTy pid (Just c) = prettyCompTy' pid c

        prettyCompTy' :: PkgId -> CompName -> String
        prettyCompTy' pid CompNameLib        = prettyPid pid
        prettyCompTy' _pid CompNameSetup     = "[setup]"
        prettyCompTy' pid (CompNameExe n)    = "[" ++ prettyPid pid ++ ":exe:"   ++ show n ++ "]"
        prettyCompTy' pid (CompNameTest n)   = "[" ++ prettyPid pid ++ ":test:"  ++ show n ++ "]"
        prettyCompTy' pid (CompNameBench n)  = "[" ++ prettyPid pid ++ ":bench:" ++ show n ++ "]"
        prettyCompTy' pid (CompNameSubLib n) = "[" ++ prettyPid pid ++ ":lib:" ++ show n ++ "]"
        prettyCompTy' pid (CompNameFLib n)   = "[" ++ prettyPid pid ++ ":flib:" ++ show n ++ "]"

reductionClosureAM
    :: PlanJson
    -> Either [DotUnitId] (Map DotUnitId (Set DotUnitId), Map DotUnitId (Set DotUnitId))
reductionClosureAM plan = TG.runG am $ \g ->
    (TG.adjacencyMap (TG.reduction g), am)
  where
    am = planJsonDotUnitGraph plan

-------------------------------------------------------------------------------
-- Diff
-------------------------------------------------------------------------------

data DotPkgName = DPN !PkgName (Maybe CompName)
  deriving (Eq, Ord, Show)

data DiffOp = Removed | Changed | Added

-- quantified name to silent redundant import warning
instance Data.Semigroup.Semigroup DiffOp where
    Changed <> x = x
    x <> _       = x

doDiff :: UseColors -> PlanJson -> PlanJson -> IO ()
doDiff useColors oldPlan newPlan = runCWriterIO useColors (dumpDiff oldPlan newPlan)

dumpDiff :: PlanJson -> PlanJson -> CWriter ()
dumpDiff oldPlan newPlan = case liftA2 (,) (reductionClosureAM oldPlan) (reductionClosureAM newPlan) of
    Left  xs       -> loopGraph xs
    Right ((old, oldC), (new, newC)) -> do
        let oldPkgs, newPkgs :: Map DotPkgName Ver
            oldPkgs = M.fromList $ map (fromUnitId oldPm . fst) $ M.toList old
            newPkgs = M.fromList $ map (fromUnitId newPm . fst) $ M.toList new

        let alignedPkgs = align oldPkgs newPkgs

        unless (oldPkgs == newPkgs) $ do
            putCTextLn ""
            putCTextLn "Package versions"
            putCTextLn "~~~~~~~~~~~~~~~~"
            putCTextLn ""

            ifor_ alignedPkgs $ \(DPN pn cn) vers -> do
                let emphasise' | pn `S.member` localPackages             = underline
                               | pn `S.member` directDepsOfLocalPackages = emphasise
                               | otherwise                               = id

                let putLine b v = putCTextLn $ colorifyText c s <> emphasise' (fromText (dispPkgId (PkgId pn v)) <> fromText (maybe "" (\cn' -> " " <> dispCompName cn') cn))
                      where
                        c = if b then Green else Red
                        s = if b then "+" else "-"

                let putLine2 b v = putCTextLn $ colorifyText c s <> emphasise' (fromText (prettyPkgName pn) <> "-" <> colorifyText c (dispVer v) <> fromText (maybe "" (\cn' -> " " <> dispCompName cn') cn))
                      where
                        c = if b then Green else Red
                        s = if b then "+" else "-"

                case vers of
                    This o    -> putLine False o
                    That n    -> putLine True n
                    These o n
                        | o == n    -> pure ()
                        | otherwise -> putLine2 False o >> putLine2 True n

        let mk :: Map UnitId Unit
               -> Map DotUnitId (Set DotUnitId)
               -> Map DotPkgName (Set DotPkgName)
            mk pm input = M.fromList
                [ (fromUnitId' pm k, S.map (fromUnitId' pm) vs)
                | (k, vs) <- M.toList input
                ]

        let oldAm, oldAmC, newAm, newAmC :: Map DotPkgName (Set DotPkgName)
            oldAm = mk oldPm old
            newAm = mk newPm new
            oldAmC = mk oldPm oldC
            newAmC = mk newPm newC

        unless (oldAm == newAm) $ do
            putCTextLn ""
            putCTextLn "Dependency graph"
            putCTextLn "~~~~~~~~~~~~~~~~"
            putCTextLn ""

            let am       = align oldAm newAm
            let nonRoots = mconcat (M.elems oldAm) <> mconcat (M.elems newAm)
            let roots    = M.keysSet am `S.difference` nonRoots

            evalStateT (mapM_ (go1 alignedPkgs oldAmC newAmC am) roots) S.empty
  where
    oldPm = pjUnits oldPlan
    newPm = pjUnits newPlan

    localPackages :: Set PkgName
    localPackages = S.fromList
        [ pn
        | u <- M.elems oldPm ++ M.elems newPm
        , uType u == UnitTypeLocal
        , let PkgId pn _ = uPId u
        ]

    directDepsOfLocalPackages :: Set PkgName
    directDepsOfLocalPackages = S.fromList $ catMaybes
        [ nameFromId . uPId <$> (M.lookup depUid oldPm <|> M.lookup depUid newPm)
        | u <- M.elems oldPm ++ M.elems newPm
        , uType u == UnitTypeLocal
        , ci <- M.elems (uComps u)
        , depUid <- S.toList (ciLibDeps ci)
        ]
      where
        nameFromId (PkgId pn _) = pn


    fromUnitId :: Map UnitId Unit -> DotUnitId -> (DotPkgName, Ver)
    fromUnitId db (DU uid cn) = case M.lookup uid db of
        Nothing -> error $ "Unknown unit-id " ++ show uid
        Just Unit { uPId = PkgId pn ver } -> (DPN pn cn, ver)

    fromUnitId' :: Map UnitId Unit -> DotUnitId -> DotPkgName
    fromUnitId' db = fst . fromUnitId db

    loopGraph :: [DotUnitId] -> CWriter ()
    loopGraph xs = do
        putCTextLn $ colorifyStr Red $ "panic: Found a loop"
        mapM_ (putCTextLn . fromString . show) xs

    go1 :: Map DotPkgName (These Ver Ver)
        -> Map DotPkgName (Set DotPkgName)
        -> Map DotPkgName (Set DotPkgName)
        -> Map DotPkgName (These (Set DotPkgName) (Set DotPkgName))
        -> DotPkgName
        -> StateT (Set DotPkgName) CWriter ()
    go1 alignedPkgs oldAmC newAmC am = go2 Changed [] where
        go2 :: DiffOp
            -> [(Maybe CompName, Bool)]
            -> DotPkgName
            -> StateT (Set DotPkgName) CWriter ()
        go2 op' lvl dpn@(DPN pn comp) = do
            let emphasise' | pn `S.member` localPackages             = underline
                           | pn `S.member` directDepsOfLocalPackages = emphasise
                           | otherwise                               = id


            let deps = M.lookup dpn am
            let odepsC = M.findWithDefault S.empty dpn oldAmC
            let ndepsC = M.findWithDefault S.empty dpn newAmC

            let (op, odeps, ndeps) = case deps of
                    -- when a dependency is added or removed we won't print its dependencies.
                    Nothing          -> (op', mempty, mempty)
                    Just (This _)    -> (op' <> Removed, mempty, mempty)
                    Just (That _)    -> (op' <> Added, mempty, mempty)
                    Just (These o n) -> (op', o, n)

            let putStrLn' :: MonadCWriter m => CText -> CText -> m ()
                putStrLn' pfx s = putCTextLn $ case op of
                    Changed -> "    " <> pfx <> s
                    Added   -> colorifyText Green "+++ " <> pfx <> recolorify Green s
                    Removed -> colorifyText Red   "--- " <> pfx <> recolorify Red   s

            seen <- gets (S.member dpn)
            modify' (S.insert dpn)

            let pn_label :: CText
                pn_label = emphasise' $ fromText (prettyCompTy pn comp) <> case (op, M.lookup dpn alignedPkgs) of
                    (_, Nothing) -> ""
                    (_, Just (This ver))  -> " " <> fromText (dispVer ver) <> " -> "
                    (_, Just (That ver))  -> " -> " <> fromText (dispVer ver)
                    (Changed, Just (These o n))
                        | o == n -> "-" <> fromText (dispVer o)
                        | otherwise  -> " " <> colorifyText Red (dispVer o) <> " -> " <> colorifyText Green (dispVer n)
                    (Added, Just (These o n))
                        | o == n -> "-" <> fromText (dispVer o)
                        | otherwise -> " -> " <> fromText (dispVer n)
                    (Removed, Just (These o n))
                        | o == n -> "-" <> fromText (dispVer o)
                        | otherwise -> " " <> fromText (dispVer o) <> " ->"

            if seen
            then putStrLn' (linepfx lvl) $ pn_label <> " ┄┄"
            else do
                putStrLn' (linepfx lvl) pn_label
                for_ (lastAnn $ S.toList $ odeps <> ndeps) $ \(l, depDpn) -> do
                    let depOp | S.member depDpn odeps && not (S.member depDpn ndepsC) = Removed
                              | S.member depDpn ndeps && not (S.member depDpn odepsC) = Added
                              | otherwise = Changed

                    go2 depOp (lvl ++ [(comp, not l)]) depDpn

        linepfx :: [(Maybe CompName, Bool)] -> CText
        linepfx lvl = case unsnoc lvl of
           Nothing -> mempty
           Just (xs,(_,z)) -> mconcat [ if x then " │ " else "   " | (_,x) <- xs ]
                              <> (if z then " ├─ " else " └─ ")

    prettyPkgName (PkgName pn) = pn

    prettyCompTy :: PkgName -> Maybe CompName -> T.Text
    prettyCompTy pn Nothing  = "[" <> prettyPkgName pn <> ":all]"
    prettyCompTy pn (Just c) = prettyCompTy' pn c

    prettyCompTy' :: PkgName -> CompName -> T.Text
    prettyCompTy' pn CompNameLib        = prettyPkgName pn
    prettyCompTy' _pn CompNameSetup     = "[setup]"
    prettyCompTy' pn (CompNameExe n)    = "[" <> prettyPkgName pn <> ":exe:"   <> n <> "]"
    prettyCompTy' pn (CompNameTest n)   = "[" <> prettyPkgName pn <> ":test:"  <> n <> "]"
    prettyCompTy' pn (CompNameBench n)  = "[" <> prettyPkgName pn <> ":bench:" <> n <> "]"
    prettyCompTy' pn (CompNameSubLib n) = "[" <> prettyPkgName pn <> ":lib:" <> n <> "]"
    prettyCompTy' pn (CompNameFLib n)   = "[" <> prettyPkgName pn <> ":flib:" <> n <> "]"

-------------------------------------------------------------------------------
-- Dot
-------------------------------------------------------------------------------

-- | vertex of dot graph.
--
-- if @'Maybe' 'CompName'@ is Nothing, this is legacy, multi-component unit.
data DotUnitId = DU !UnitId (Maybe CompName)
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

doDot
    :: Flag ShowBuiltin
    -> Flag ShowGlobal
    -> PlanJson
    -> Flag DotTred
    -> Flag DotTredWght
    -> [Highlight] -> IO ()
doDot showBuiltin showGlobal plan tred tredWeights highlights = either loopGraph id $ TG.runG am $ \g' -> do
    let g = if fromFlag DotTred tred then TG.reduction g' else g'

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
                            (TG.gVertexIndex g b + TG.gVertexIndex g a * len)

            U.freeze v

    let weights :: Map (DotUnitId, DotUnitId) Double
        weights =
            if fromFlag DotTred tred && fromFlag DotTredWght tredWeights
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
            UnitTypeBuiltin -> fromFlag ShowBuiltin showBuiltin
            UnitTypeGlobal  -> fromFlag ShowGlobal showGlobal
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
        Just unit ->
            let PkgId pn _ = uPId unit in
            dispPkgId (uPId unit) <> maybe ":*" (dispCompName' pn) mcname

    dispCompName' :: PkgName -> CompName -> T.Text
    dispCompName' _ CompNameLib = ""
    dispCompName' pn cname      = ":" <> dispCompNameTarget pn cname

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

        let PkgId pn _ = uPId
            g = dispCompNameTarget pn cn

        guard (getAny $ checkPattern pat pn cn)

        pure (g, uId, cn)


-------------------------------------------------------------------------------
-- topo
-------------------------------------------------------------------------------

doTopo
    :: UseColors
    -> Flag ShowBuiltin
    -> Flag ShowGlobal
    -> PlanJson
    -> Flag TopoReverse
    -> Flag ShowFlags
    -> IO ()
doTopo useColors showBuiltin showGlobal plan rev showFlags = do
    let units = pjUnits plan

    let topo = TG.runG (planJsonIdGraph plan) $ \TG.G {..} ->
            map gFromVertex gVertices

    let showUnit unit = case uType unit of
          UnitTypeBuiltin -> fromFlag ShowBuiltin showBuiltin
          UnitTypeGlobal  -> fromFlag ShowGlobal showGlobal
          UnitTypeLocal   -> True
          UnitTypeInplace -> True

    let rev' = if fromFlag TopoReverse rev then reverse else id

    runCWriterIO useColors $ for_ topo $ \topo' -> for_ (rev' topo') $ \unitId ->
        for_ (M.lookup unitId units) $ \unit ->
            when (showUnit unit) $ do
                let pkgIdColor = colorifyText $ case uType unit of
                        UnitTypeBuiltin -> Blue
                        UnitTypeGlobal  -> White
                        UnitTypeLocal   -> Green
                        UnitTypeInplace -> Red
                let PkgId pn _ = uPId unit
                let components = case M.keys (uComps unit) of
                        [] -> ""
                        [CompNameLib] -> ""
                        names -> " " <> T.intercalate " " (map (dispCompNameTarget pn) names)
                let flags | fromFlag ShowFlags showFlags = fromString $ concat
                        [ " "
                        ++ (if flagValue then "+" else "-")
                        ++ T.unpack flagName
                        | (FlagName flagName, flagValue) <- M.toList (uFlags unit)
                        ]
                          | otherwise = ""
                putCTextLn $
                    pkgIdColor (dispPkgId $ uPId unit)
                    <> fromString (T.unpack components)
                    <> flags

----------------------------------------------------------------------------

dumpPlanJson :: PlanJson -> CWriter ()
dumpPlanJson (PlanJson { pjUnits = pm }) =
    evalStateT (mapM_ (go2 []) (S.toList roots)) S.empty
  where
    id2pid :: Map UnitId PkgId
    id2pid = M.fromList [ (uId, uPId) | Unit{..} <- M.elems pm ]

    lupPid uid = M.findWithDefault undefined uid id2pid

    go2 :: [(CompName,Bool)] -> UnitId -> StateT (Set UnitId) CWriter ()
    go2 lvl pid = do
        pidSeen <- gets (S.member pid)

        let pid_label | preExists = fromString (prettyId pid)
                      | otherwise = colorify_ White (prettyId pid)

        if not pidSeen
         then do
            putCTextLn $ linepfx <> pid_label
            showDeps
         else do
            putCTextLn $ linepfx <> pid_label <> ccol CompNameLib " ┄┄"
            -- tell $ LT.fromString (linepfx' ++ " └┄\n")

        modify' (S.insert pid)

        return ()
      where
        Just x' = M.lookup pid pm

        preExists = uType x' == UnitTypeBuiltin

        showDeps = for_ (M.toList $ uComps x') $ \(ct,deps) -> do
            unless (ct == CompNameLib) $
                putCTextLn $ linepfx' <> " " <> prettyCompTy (lupPid pid) ct
            for_ (lastAnn $ S.toList (ciLibDeps deps)) $ \(l,y) -> do
                go2 (lvl ++ [(ct, not l)]) y


        linepfx :: CText
        linepfx = case unsnoc lvl of
            Nothing -> ""
            Just (xs,(zt,z)) -> mconcat [ if x then ccol xt " │ " else "   " | (xt,x) <- xs ]
                                        <> (ccol zt $ if z then " ├─ " else " └─ ")

        linepfx' :: CText
        linepfx' = mconcat [ if x then  " │ " else "   " | (_,x) <- lvl ]

    roots :: Set UnitId
    roots = M.keysSet pm `S.difference` leafs
      where
        leafs = mconcat $ concatMap (map (ciLibDeps . snd) . M.toList . uComps) (M.elems pm)

    prettyId :: UnitId -> String
    prettyId = prettyPid . lupPid
    prettyPid = T.unpack . dispPkgId

    prettyCompTy :: PkgId -> CompName -> CText
    prettyCompTy _pid c@CompNameLib       = ccol c "[lib]"
    prettyCompTy _pid c@CompNameSetup     = ccol c "[setup]"
    prettyCompTy pid c@(CompNameExe n)    = ccol c $ "[" ++ prettyPid pid ++ ":exe:"   ++ show n ++ "]"
    prettyCompTy pid c@(CompNameTest n)   = ccol c $ "[" ++ prettyPid pid ++ ":test:"  ++ show n ++ "]"
    prettyCompTy pid c@(CompNameBench n)  = ccol c $ "[" ++ prettyPid pid ++ ":bench:" ++ show n ++ "]"
    prettyCompTy pid c@(CompNameSubLib n) = ccol c $ "[" ++ prettyPid pid ++ ":lib:" ++ show n ++ "]"
    prettyCompTy pid c@(CompNameFLib n)   = ccol c $ "[" ++ prettyPid pid ++ ":flib:" ++ show n ++ "]"

    ccol CompNameLib        = colorifyStr White
    ccol (CompNameExe _)    = colorifyStr Green
    ccol CompNameSetup      = colorifyStr Red
    ccol (CompNameTest _)   = colorifyStr Yellow
    ccol (CompNameBench _)  = colorifyStr Cyan
    ccol (CompNameSubLib _) = colorifyStr Blue
    ccol (CompNameFLib _)   = colorifyStr Magenta

colorify_ :: Color -> String -> CText
colorify_ col s
  | haveUnderlineSupport = CText [CPiece (T.pack s) [SetUnderlining SingleUnderline, SetColor Foreground Vivid col]]
  | otherwise            = colorifyStr col s

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

-------------------------------------------------------------------------------
-- Colors
-------------------------------------------------------------------------------

data CPiece = CPiece !T.Text [SGR]
  deriving (Eq, Show)

newtype CText = CText [CPiece]
  deriving (Eq, Show)

instance IsString CText where
    fromString s
        | null s    = mempty
        | otherwise = CText [CPiece (fromString s) []]

instance Semigroup CText where
    CText xs <> CText ys = CText (xs <> ys)

instance Monoid CText where
    mempty  = CText []
    mappend = (<>)

fromText :: T.Text -> CText
fromText t = CText [CPiece t []]

colorifyStr :: Color -> String -> CText
colorifyStr c t = CText [CPiece (T.pack t) [SetColor Foreground Vivid c]]

colorifyText :: Color -> T.Text -> CText
colorifyText c t = CText [CPiece t [SetColor Foreground Vivid c]]

recolorify :: Color -> CText -> CText
recolorify c (CText xs) = CText
    [ CPiece t (SetColor Foreground Vivid c : sgr)
    | CPiece t sgr' <- xs
    , let sgr = filter notSetColor sgr'
    ]
  where
    notSetColor SetColor {} = False
    notSetColor _           = True

-- | We decide to bold, we could do something else to.
emphasise :: CText -> CText
emphasise (CText xs) = CText
    [ CPiece t (SetConsoleIntensity BoldIntensity : sgr)
    | CPiece t sgr <- xs
    ]

underline :: CText -> CText
underline (CText xs) | haveUnderlineSupport  = CText
    [ CPiece t (SetUnderlining SingleUnderline : sgr)
    | CPiece t sgr <- xs
    ]
underline x = x

-- | Colored writer (list is lines)
newtype CWriter a = CWriter { unCWriter :: Endo [CText] -> (Endo [CText], a) }
  deriving Functor

class Monad m => MonadCWriter m where
    putCTextLn :: CText -> m ()

instance MonadCWriter CWriter where
    putCTextLn t = CWriter $ \l -> (l <> Endo (t :), ())

instance MonadCWriter m => MonadCWriter (StateT s m) where
    putCTextLn = lift . putCTextLn

instance Applicative CWriter where
    pure  = return
    (<*>) = ap

instance Monad CWriter where
    return x = CWriter $ \ls -> (ls, x)

    m >>= k = CWriter $ \ls0 ->
        let (ls1, x) = unCWriter m ls0
        in unCWriter (k x) ls1

data UseColors = ColorsNever | ColorsAuto | ColorsAlways
  deriving (Eq, Show)

runCWriterIO :: UseColors -> CWriter () -> IO ()
runCWriterIO ColorsNever  m = runCWriterIONoColors m
runCWriterIO ColorsAlways m = runCWriterIOColors m
runCWriterIO ColorsAuto   m = do
    supports <- hSupportsANSIColor stdout
    if supports
    then runCWriterIOColors m
    else runCWriterIONoColors m

runCWriterIOColors :: CWriter () -> IO ()
runCWriterIOColors (CWriter f) =
    forM_ (appEndo (fst (f mempty)) []) $ \(CText l) -> do
        forM_ l $ \(CPiece t sgr) -> do
            unless (null sgr) $ setSGR sgr
            T.putStr t
            unless (null sgr) $ setSGR []
        putChar '\n'

runCWriterIONoColors :: CWriter () -> IO ()
runCWriterIONoColors (CWriter f) =
    forM_ (appEndo (fst (f mempty)) []) $ \(CText l) -> do
        forM_ l $ \(CPiece t _) -> T.putStr t
        putChar '\n'
