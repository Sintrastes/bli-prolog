
module Bli.Prolog.SearchStrategies where

----------------------------------------------------------------------
-- Traveral of Search Trees
----------------------------------------------------------------------

import Bli.Prolog.Interp.Data
import Data.Data
import Data.Typeable

-- | An ADT representing all of the different search 
--   algorithms bli prolog can be configured to run with.
data Search = DFS | BFS | Limited
            deriving (Show, Eq, Data, Typeable)

-- | Helper function for converting our Search ADT into
--   actual search functions.
searchFunction DFS _     = dfs
searchFunction BFS _     = bfs
searchFunction Limited n = limitedDfs n

-- | Depth first search function.
dfs :: SearchTree -> [Solution]
dfs (Sol sols) = [sols]
dfs (Node _ st) = [ s | t <- st, s <- dfs t]

-- | Breath first search function.
bfs :: SearchTree -> [Solution]
bfs t = trav [t]
    where trav [] = []
          trav ((Sol x) : q) = x : trav q
          trav ((Node _ st)  : q) = trav (q ++ st)

-- | Limited depth first search function.
limitedDfs :: Int -> SearchTree -> [Solution]
limitedDfs _ (Sol sols)  = [sols]
limitedDfs 0 _           = []
limitedDfs n (Node _ st) = [ s | t <- st, s <- limitedDfs (n-1) t]
