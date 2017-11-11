{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.RWS.Strict
import           Data.Foldable            (Foldable (..), asum)
import qualified Data.Graph               as G
import           Data.List                (isPrefixOf, isInfixOf, isSuffixOf)
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as LT
import qualified Data.Text.Lazy.IO        as LT
import           Options.Applicative
import           System.Console.ANSI
import           System.Exit              (exitFailure)
import           System.IO                (hPutStrLn, stderr)

import           Cabal.Plan

haveUnderlineSupport :: Bool
#if defined(UNDERLINE_SUPPORT)
haveUnderlineSupport = True
#else
haveUnderlineSupport = False
#endif

data GlobalOptions = GlobalOptions
     { buildDir :: Maybe FilePath
     , cmd :: Command
     }

data Command
    = InfoCommand
    | ShowCommand
    | FingerprintCommand
    | ListBinsCommand MatchCount MatchPref [Pattern]
    | DotCommand

main :: IO ()
main = do
    GlobalOptions{..} <- execParser $ info (optParser <**> helper) fullDesc
    val@(plan, _) <- findAndDecodePlanJson buildDir
    case cmd of
      InfoCommand -> doInfo val
      ShowCommand -> print val
      ListBinsCommand count pref pats -> do
          let bins = doListBin plan pref pats
          case (count, bins) of
              (MatchMany, _) -> forM_ bins $ \(g, fn) ->
                    putStrLn (g ++ "  " ++ fn)
              (MatchOne, [(_,p)]) -> putStrLn p
              (MatchOne, []) -> do
                 hPutStrLn stderr "No matches found."
                 exitFailure
              (MatchOne, _) -> do
                 hPutStrLn stderr "Found more than one matching pattern:"
                 forM_ bins $ \(p,_) -> hPutStrLn stderr $ "  " ++ p
                 exitFailure
      FingerprintCommand -> doFingerprint plan
      DotCommand -> doDot plan
  where
    optParser = GlobalOptions <$> dirParser <*> (cmdParser <|> defaultCommand)
    dirParser = optional . strOption $ mconcat
        [ long "builddir", metavar "DIR"
        , help "Build directory to read plan.json from." ]

    subCommand name desc val = command name $ info val (progDesc desc)

    patternParser = argument (Pattern <$> str) . mconcat

    cmdParser = subparser $ mconcat
        [ subCommand "info" "Info" $ pure InfoCommand
        , subCommand "show" "Show" $ pure ShowCommand
        , subCommand "list-bins" "List All Binaries" .
            listBinParser MatchMany . many $ patternParser
                [ metavar "PATTERNS...", help "Patterns to match." ]
        , subCommand "list-bin" "List Single Binary" .
            listBinParser MatchOne $ pure <$> patternParser
                [ metavar "PATTERN", help "Pattern to match." ]
        , subCommand "fingerprint" "Fingerprint" $ pure FingerprintCommand
        , subCommand "dot" "Dependency .dot" $ pure DotCommand
        ]
    defaultCommand = pure InfoCommand

data Pattern = Pattern String
    deriving (Show, Eq)

data MatchCount = MatchOne | MatchMany
    deriving (Show, Eq)

data MatchPref = Prefix | Infix | Suffix | Exact
    deriving (Show, Eq)

listBinParser
    :: MatchCount
    -> Parser [Pattern]
    -> Parser Command
listBinParser count pats
    = ListBinsCommand count <$> matchPrefParser <*> pats <**> helper
  where
    matchPrefParser :: Parser MatchPref
    matchPrefParser = asum [exact, prefix, infix_, suffix, pure Exact ]

    exact :: Parser MatchPref
    exact = flag' Exact $ mconcat
        [ long "exact" , help "Use exact match for pattern." ]

    prefix :: Parser MatchPref
    prefix = flag' Prefix $ mconcat
        [ long "prefix" , help "Use prefix match for pattern." ]

    infix_ :: Parser MatchPref
    infix_ = flag' Infix $ mconcat
        [ long "infix" , help "Use infix match for pattern." ]

    suffix :: Parser MatchPref
    suffix = flag' Suffix $ mconcat
        [ long "suffix" , help "Use suffix match for pattern." ]

checkPattern :: MatchPref -> Pattern -> String -> Any
checkPattern pref (Pattern p) s = Any $ compareFun p s
  where
    compareFun = case pref of
        Prefix -> isPrefixOf
        Infix -> isInfixOf
        Suffix -> isSuffixOf
        Exact -> (==)

doListBin :: PlanJson -> MatchPref -> [Pattern] -> [(String, FilePath)]
doListBin plan pref patterns = do
    (_, Unit{..}) <- M.toList $ pjUnits plan
    (cn, ci) <- M.toList $ uComps
    case ciBinFile ci of
        Nothing -> []
        Just fn -> do
            let PkgId (PkgName pn) _ = uPId
                g = case cn of
                    CompNameLib -> T.unpack (pn <> T.pack":lib:" <> pn)
                    _           -> T.unpack (pn <> T.pack":" <> dispCompName cn)
            guard . getAny $ patternChecker g
            [(g, fn)]
  where
    patternChecker :: String -> Any
    patternChecker = case patterns of
        [] -> const $ Any True
        _ -> mconcat $ map (checkPattern pref) patterns

doFingerprint :: PlanJson -> IO ()
doFingerprint plan = do
    let pids = M.fromList [ (uPId u, u) | (_,u) <- M.toList (pjUnits plan) ]

    forM_ (M.toList pids) $ \(_,Unit{..}) -> do
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
    forM_ xs print

    putStrLn ""
    putStrLn "Direct deps"
    putStrLn "~~~~~~~~~~~"
    putStrLn ""

    let locals = [ Unit{..} | Unit{..} <- M.elems pm, uType == UnitTypeLocal ]
        pm = pjUnits plan

    forM_ locals $ \pitem -> do
        print (uPId pitem)
        forM_ (M.toList $ uComps pitem) $ \(ct,ci) -> do
            print ct
            forM_ (S.toList $ ciLibDeps ci) $ \dep -> do
                let Just dep' = M.lookup dep pm
                    pid = uPId dep'
                putStrLn ("  " ++ T.unpack (dispPkgId pid))
            putStrLn ""

    return ()

doDot :: PlanJson -> IO ()
doDot plan = do
    putStrLn "digraph plan {"
    putStrLn "overlap = false;"

    let units = pjUnits plan

    -- vertices
    forM_ units $ \unit ->
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
    forM_ units $ \unit -> do
        let deps = foldMap (\ci -> ciLibDeps ci <> ciExeDeps ci) (uComps unit)

        forM_ deps $ \depUId -> forM_ (M.lookup depUId units) $ \dunit ->
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

        showDeps = forM_ (M.toList $ uComps x') $ \(ct,deps) -> do
            unless (ct == CompNameLib) $
                tell (LT.fromString $ linepfx' ++ " " ++ prettyCompTy (lupPid pid) ct ++ "\n")
            forM_ (lastAnn $ S.toList (ciLibDeps deps)) $ \(l,y) -> do
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
