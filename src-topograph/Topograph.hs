{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
-- |
--
-- SPDX-License-Id: BSD-3-Clause
-- Author: Oleg Grenrus
--
module Topograph where

import           Data.Orphans             ()
import qualified Data.Graph               as G
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Vector as V

-- | Graph representation.
data G v a = G
    { gVertices   :: [a]            -- ^ all vertices, in topological order
    , gFromVertex :: a -> v         -- ^ retrieve original vertex data. /O(1)/
    , gDiff       :: a -> a -> Int  -- ^ Upper bound of the path length. Negative if there aren't path. /O(1)/
    }

-- | Run action on topologically sorted representation of the graph.
runG
    :: forall v r. Ord v
    => Map v (Set v)                    -- ^ Adjacency Map
    -> (forall i. Ord i => G v i -> r)  -- ^ function on linear indices
    -> Either [v] r                     -- ^ Return the result or a cycle in the graph.
runG m f
    -- TODO: check that graph is DAG
    | otherwise = Right (f g)
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

    g :: G v Int
    g = G
        { gVertices   = [0 .. V.length indices - 1]
        , gFromVertex = (indices V.!)
        , gDiff       = \a b -> b - a
        }
