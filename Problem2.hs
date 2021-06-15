module Problem2 where

import Data.Maybe ( fromJust )
import DurationMonad ( Duration(..), getDuration, getValue )
import Problem1 ( Path, Node(..), adj, adjacentNodes, allNodes )



{- 1. *Labelled* graph structure: nodes and labelled adjacency matrix
(i.e. the labelled edges of the graph)
-}
adjT :: (Node,Node) -> Maybe Int
adjT p = case p of
           (A,B) -> Just 2
           (A,C) -> Just 3
           (A,F) -> Just 6
           (B,A) -> Just 30
           (B,C) -> Just 0
           (B,E) -> Just 4
           (B,F) -> Just 3
           (C,A) -> Just 60
           (C,B) -> Just 3
           (C,D) -> Just 50
           (D,C) -> Just 2
           (D,E) -> Just 3
           (E,B) -> Just 1
           (E,D) -> Just 3
           (E,F) -> Just 2
           (F,A) -> Just 4
           (F,B) -> Just 5
           (F,E) -> Just 3
           (_,_) -> Nothing

-- 2. Auxiliary functions
{- Given a node n and a list of nodes ns the function returns the nodes
in ns that can be reached from n in one step together with the time
necessary to reach them.
-}
tadjacentNodes :: Node -> [Node] -> [Duration Node]
tadjacentNodes n ns = map (\x -> Duration (fromJust (adjT (n, x)), x)) filtered
                      where filtered = filter (\x -> adj(n,x)) ns

-- 3. Main body
{- For each node a in ns, if a is not already in p the function creates
   a new path (like in the previous problem) and computes its cost.
-}
taddToEnd :: Duration Path -> [Duration Node] -> [Duration Path]
taddToEnd p ns | null as = [p]
               | otherwise = map (\x -> Duration(getDuration x + getDuration p, getValue p ++ [getValue x]) ) as
              where filteredList = filter ( `notElem` getValue p) (adjacentNodes (last (getValue p)) (map getValue ns))
                    as = tadjacentNodes (last(getValue p)) filteredList


-- Computes all Hamiltonian cycles with associated cost starting from a given node.
hCyclesCost :: Node -> [Duration Path]
hCyclesCost n = map (\d -> Duration(getDuration d + fromJust (adjT (last (getValue  d), n)) ,  getValue d ++ [n]) ) t
              where x = hCyclesAuxCost (length allNodes) [Duration(0,[n])]
                    t = filter (\l -> adj(last (getValue l), n) && length (getValue l) == length allNodes) x


hCyclesAuxCost :: Int -> [Duration Path] -> [Duration Path]
hCyclesAuxCost 0 x = x
hCyclesAuxCost i x = hCyclesAuxCost (i-1) (concatMap (`taddToEnd` allt) x)
                   where allt = map (\y -> Duration(0, y)) allNodes



-- The main program.                                 
tsp = minimum . hCyclesCost
