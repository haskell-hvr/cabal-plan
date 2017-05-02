{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.RWS.Strict
import qualified Data.Graph               as G
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Semigroup           ((<>))
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Builder   as LT
import qualified Data.Text.Lazy.IO        as LT
import           Options.Applicative
import           System.Console.ANSI
import           System.Environment

import           Cabal.Plan

main :: IO ()
main = do
    -- TODO: optparse-applicative
    args <- getArgs
    case args of
      [] -> doInfo
      ("info":_) -> doInfo
      ("show":_) -> print =<< findAndDecodePlanJson
      ("list-bin":_) -> doListBin
      ("fingerprint":_) -> doFingerprint
      _ -> fail "unknown command (known commands: info show list-bin fingerprint)"

main' :: IO ()
main' = do
    cmd <- execParser opts
    case cmd of
      InfoCommand -> doInfo
  where
    opts = subparser
      (  command "info" (info infoOptions (progDesc "Info")) )

commandParser :: Parser Command
commandParser = pure InfoCommand

data Command = InfoCommand
  deriving (Show, Eq)
  
doListBin :: IO ()
doListBin = do
    (v,_projbase) <- findAndDecodePlanJson

    forM_ (M.toList (pjUnits v)) $ \(_,Unit{..}) -> do
        forM_ (M.toList uComps) $ \(cn,ci) -> do
            case ciBinFile ci of
              Nothing -> return ()
              Just fn -> do
                  let PkgId (PkgName pn) _ = uPId
                      g = case cn of
                            CompNameLib -> T.unpack (pn <> T.pack":lib:" <> pn)
                            _           -> T.unpack (pn <> T.pack":" <> dispCompName cn)
                  putStrLn (g ++ "  " ++ fn)

doFingerprint :: IO ()
doFingerprint = do
    (v,_projbase) <- findAndDecodePlanJson

    let pids = M.fromList [ (uPId u, u) | (_,u) <- M.toList (pjUnits v) ]

    forM_ (M.toList pids) $ \(_,Unit{..}) -> do
        let h = maybe "________________________________________________________________"
                      dispSha256 $ uSha256
        case uType of
          UnitTypeBuiltin -> T.putStrLn (h <> " B " <> dispPkgId uPId)
          UnitTypeGlobal  -> T.putStrLn (h <> " G " <> dispPkgId uPId)
          UnitTypeLocal   -> T.putStrLn (h <> " L " <> dispPkgId uPId)
          UnitTypeInplace -> T.putStrLn (h <> " I " <> dispPkgId uPId)

doInfo :: IO ()
doInfo = do
    (v,projbase) <- findAndDecodePlanJson

    putStrLn ("using '" ++ projbase ++ "' as project root")
    putStrLn ""
    putStrLn "Tree"
    putStrLn "~~~~"
    putStrLn ""
    LT.putStrLn (dumpPlanJson v)

    -- print (findCycles (planJsonIdGrap v))

    putStrLn ""
    putStrLn "Top-sorted"
    putStrLn "~~~~~~~~~~"
    putStrLn ""

    let xs = toposort (planJsonIdGraph v)
    forM_ xs print

    putStrLn ""
    putStrLn "Direct deps"
    putStrLn "~~~~~~~~~~~"
    putStrLn ""

    let locals = [ Unit{..} | Unit{..} <- M.elems pm, uType == UnitTypeLocal ]
        pm = pjUnits v

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

        let pid_label = if preExists then (prettyId pid) else colorify White (prettyId pid)

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
    prettyCompTy pid c@(CompNameExe n)   = ccol c $ "[" ++ prettyPid pid ++ ":exe:"   ++ show n ++ "]"
    prettyCompTy pid c@(CompNameTest n)  = ccol c $ "[" ++ prettyPid pid ++ ":test:"  ++ show n ++ "]"
    prettyCompTy pid c@(CompNameBench n) = ccol c $ "[" ++ prettyPid pid ++ ":bench:" ++ show n ++ "]"
    prettyCompTy pid c@(CompNameSubLib n) = ccol c $ "[" ++ prettyPid pid ++ ":lib:" ++ show n ++ "]"

    ccol CompNameLib        = colorify White
    ccol (CompNameExe _)    = colorify Green
    ccol CompNameSetup      = colorify Red
    ccol (CompNameTest _)   = colorify Yellow
    ccol (CompNameBench _)  = colorify Cyan
    ccol (CompNameSubLib _) = colorify Blue


colorify :: Color -> String -> String
colorify col s = setSGRCode [SetColor Foreground Vivid col] ++ s ++ setSGRCode [Reset]


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
