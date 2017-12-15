{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.RWS.Strict
import           Data.Foldable            (Foldable (..), for_)
import           Data.Char                (isAlphaNum)
import qualified Data.Graph               as G
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as LT
import qualified Data.Text.Lazy.IO        as LT
import           Data.Traversable         (Traversable (..))
import           Options.Applicative
import           System.Console.ANSI
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)
import qualified Text.Parsec              as P
import qualified Text.Parsec.String       as P

import           Cabal.Plan

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
    | DotCommand

main :: IO ()
main = do
    GlobalOptions{..} <- execParser $ info (optParser <**> helper) fullDesc
    val@(plan, _) <- findAndDecodePlanJson buildDir
    case cmd of
      InfoCommand -> doInfo val
      ShowCommand -> print val
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
      DotCommand -> doDot optsShowBuiltin optsShowGlobal plan
  where
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

    cmdParser = subparser $ mconcat
        [ subCommand "info" "Info" $ pure InfoCommand
        , subCommand "show" "Show" $ pure ShowCommand
        , subCommand "list-bins" "List All Binaries" .
            listBinParser MatchMany . many $ patternParser
                [ metavar "PATTERNS...", help "Patterns to match.", completer patternCompleter ]
        , subCommand "list-bin" "List Single Binary" .
            listBinParser MatchOne $ pure <$> patternParser
                [ metavar "PATTERN", help "Pattern to match.", completer patternCompleter ]
        , subCommand "fingerprint" "Fingerprint" $ pure FingerprintCommand
        , subCommand "dot" "Dependency .dot" $ pure DotCommand
        ]
    defaultCommand = pure InfoCommand

-- | patterns are @[[pkg:]kind;]cname@
data Pattern = Pattern (Maybe T.Text) (Maybe CompType) (Maybe T.Text)
    deriving (Show, Eq)

data CompType = CompTypeLib | CompTypeExe | CompTypeTest | CompTypeBench | CompTypeSetup
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
    toCompType "setup" = return $ CompTypeSetup
    toCompType "test"  = return $ CompTypeTest
    toCompType t = fail $ "Unknown component type: " ++ show t

patternCompleter :: Completer
patternCompleter = mkCompleter $ \pfx -> do
    (plan, _) <- findAndDecodePlanJson Nothing
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

    -- returns (full, cname) pair
    findComponents ::PlanJson -> [(T.Text, T.Text)]
    findComponents plan = do
        (_, Unit{..}) <- M.toList $ pjUnits plan
        (cn, ci) <- M.toList $ uComps
        case ciBinFile ci of
            Nothing -> []
            Just _  -> do
                let PkgId pn@(PkgName pnT) _ = uPId
                    g = case cn of
                        CompNameLib -> pnT <> T.pack":lib:" <> pnT
                        _           -> pnT <> T.pack":" <> dispCompName cn

                let cnT = extractCompName pn cn
                [ (g, cnT) ]

compNameType :: CompName -> CompType
compNameType CompNameLib        = CompTypeLib
compNameType (CompNameSubLib _) = CompTypeLib
compNameType (CompNameExe _)    = CompTypeExe
compNameType (CompNameTest _)   = CompTypeTest
compNameType (CompNameBench _)  = CompTypeBench
compNameType CompNameSetup      = CompTypeSetup

data MatchCount = MatchOne | MatchMany
    deriving (Show, Eq)

listBinParser
    :: MatchCount
    -> Parser [Pattern]
    -> Parser Command
listBinParser count pats
    = ListBinsCommand count <$> pats <**> helper

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
extractCompName _            (CompNameExe cn)    = cn
extractCompName _            (CompNameTest cn)   = cn
extractCompName _            (CompNameBench cn)  = cn

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

doInfo :: (PlanJson, FilePath) -> IO ()
doInfo (plan,projbase) = do
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

doDot :: Bool -> Bool -> PlanJson -> IO ()
doDot showBuiltin showGlobal plan = do
    putStrLn "digraph plan {"
    putStrLn "overlap = false;"

    let units0 = pjUnits plan
        units1 | showBuiltin = units0
               | otherwise   = M.filter ((UnitTypeBuiltin /=) . uType) units0
        units2 | showGlobal  = units1
               | otherwise   = M.filter ((UnitTypeGlobal  /=) . uType) units1
        units = units2

    -- vertices
    for_ units $ \unit ->
        T.putStrLn $ mconcat
            [ "\""
            , dispPkgId (uPId unit)
            , "\""
            , case uType unit of
                UnitTypeBuiltin -> " [shape=octagon]"
                UnitTypeGlobal  -> " [shape=box]"
                UnitTypeLocal   -> " [shape=oval]"
                UnitTypeInplace -> " [shape=oval]"
            , ";"
            ]

    -- edges
    for_ units $ \unit -> do
        let deps = foldMap (\ci -> ciLibDeps ci <> ciExeDeps ci) (uComps unit)

        for_ deps $ \depUId -> for_ (M.lookup depUId units) $ \dunit ->
            T.putStrLn $ mconcat
                [ "\""
                , dispPkgId (uPId unit)
                , "\""
                , " -> "
                , "\""
                , dispPkgId (uPId dunit)
                , "\""
                , ";"
                ]

    putStrLn "}"

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

    ccol CompNameLib        = colorify White
    ccol (CompNameExe _)    = colorify Green
    ccol CompNameSetup      = colorify Red
    ccol (CompNameTest _)   = colorify Yellow
    ccol (CompNameBench _)  = colorify Cyan
    ccol (CompNameSubLib _) = colorify Blue

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
