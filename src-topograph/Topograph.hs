{-# LANGUAGE RankNTypes, ScopedTypeVariables, RecordWildCards #-}
-- |
--
-- SPDX-License-Id: BSD-3-Clause
-- Author: Oleg Grenrus
--
module Topograph where

import           Prelude ()
import           Prelude.Compat
import           Data.Orphans ()

import           Data.Maybe               (mapMaybe)
import           Data.Monoid              (First (..))
import           Data.List                (sort)
import qualified Data.Graph               as G
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Vector              as V

import Debug.Trace

-- | Graph representation.
data G v a = G
    { gVertices   :: [a]             -- ^ all vertices, in topological order
    , gFromVertex :: a -> v          -- ^ retrieve original vertex data. /O(1)/
    , gToVertex   :: v -> Maybe a    -- ^ /O(log n)/
    , gEdges      :: a -> [a]        -- ^ Outgoing edges.
    , gDiff       :: a -> a -> Int   -- ^ Upper bound of the path length. Negative if there aren't path. /O(1)/
    }

-- | Run action on topologically sorted representation of the graph.
--
-- === __Examples__
--
-- ==== Topological sorting
--
-- >>> runG example $ \G {..} -> map gFromVertex gVertices
-- Right "axbde"
--
-- Vertices are sorted
--
-- >>> runG example $ \G {..} -> map gFromVertex $ sort gVertices
-- Right "axbde"
--
-- ==== Outgoing edges
--
-- >>> runG example $ \G {..} -> map (map gFromVertex . gEdges) gVertices
-- Right ["xbde","de","d","e",""]
--
-- Note: edges are always larger than source vertex:
--
-- >>> runG example $ \G {..} -> getAll $ foldMap (\a -> foldMap (\b -> All (a < b)) (gEdges a)) gVertices
-- Right True
--
-- ==== Not DAG
--
-- >>> let loop = M.map S.fromList $ M.fromList [('a', "bx"), ('b', "cx"), ('c', "ax"), ('x', "")]
-- >>> runG loop $ \G {..} -> map gFromVertex gVertices
-- Left "abc"
--
-- >>> runG (M.singleton 'a' (S.singleton 'a')) $ \G {..} -> map gFromVertex gVertices
-- Left "aa"
--
runG
    :: forall v r. Ord v
    => Map v (Set v)                    -- ^ Adjacency Map
    -> (forall i. Ord i => G v i -> r)  -- ^ function on linear indices
    -> Either [v] r                     -- ^ Return the result or a cycle in the graph.
runG m f
    | Just l <- loop = Left (map (indices V.!) l)
    | otherwise      = Right (f g)
  where
    gr :: G.Graph
    r  :: G.Vertex -> ((), v, [v])
    _t  :: v -> Maybe G.Vertex

    (gr, r, _t) = G.graphFromEdges [ ((), v, S.toAscList us) | (v, us) <- M.toAscList m ]

    r' :: G.Vertex -> v
    r' i = case r i of (_, v, _) -> v

    topo :: [G.Vertex]
    topo = G.topSort gr

    indices :: V.Vector v
    indices = V.fromList (map r' topo)

    revIndices :: Map v Int
    revIndices = M.fromList $ zip (map r' topo) [0..]

    edges :: V.Vector [Int]
    edges = V.map
        (\v -> maybe
            []
            (\sv -> sort $ mapMaybe (\v' -> M.lookup v' revIndices) $ S.toList sv)
            (M.lookup v m))
        indices

    -- TODO: let's see if this check is too expensive
    loop :: Maybe [Int]
    loop = getFirst $ foldMap (\a -> foldMap (check a) (gEdges g a)) (gVertices g)
      where
        check a b
            | a < b     = First Nothing
            -- TODO: here we could use shortest path
            | otherwise = First $ case allPaths g b a of
                []      -> Nothing
                (p : _) -> Just p

    g :: G v Int
    g = G
        { gVertices   = [0 .. V.length indices - 1]
        , gFromVertex = (indices V.!)
        , gToVertex   = (`M.lookup` revIndices)
        , gDiff       = \a b -> b - a
        , gEdges      = (edges V.!)
        }

-- |
--
-- >>> runG example $ \g@G{..} -> (fmap . map . map) gFromVertex $ allPaths g <$> (gToVertex 'a') <*> (gToVertex 'e')
-- Right (Just ["axde","axe","abde","ade","ae"])
--
-- >>> runG example $ \g@G{..} -> (fmap . map . map) gFromVertex $ allPaths g <$> (gToVertex 'a') <*> (gToVertex 'a')
-- Right (Just [])
--
allPaths :: forall v a. Ord a => G v a -> a -> a -> [[a]]
allPaths G {..} a b = concatMap (map (a:) . go) (gEdges a) where
    go :: a -> [[a]]
    go i
        | i == b = [[b]]
        | otherwise = 
            let js :: [a]
                js = filter (<= b) $ gEdges i

                j2b :: [[a]]
                j2b = concatMap go js

            in map (i:) j2b

-- $setup
--
-- Graph used in examples:
--
-- @
--      a ---
--    / | \   \
--  b   |   x  \
--    \ | /   \ |
--      d ----- e
-- @
--
-- See <https://en.wikipedia.org/wiki/Transitive_reduction>
--
-- >>> let example :: Map Char (Set Char); example = M.map S.fromList $ M.fromList [('a', "bxde"), ('b', "d"), ('x', "de"), ('d', "e"), ('e', "")]
-- 
-- >>> :set -XRecordWildCards
-- >>> import Data.Monoid (All (..))
