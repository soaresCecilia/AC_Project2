module Problem2 where

import DurationMonad
import Problem1



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
eliminate :: Maybe Int -> Int
eliminate (Just a) = a
eliminate Nothing  = 0 

{- Given a node n and a list of nodes ns the function returns the nodes
in ns that can be reached from n in one step together with the time
necessary to reach them.
-}
-- !! to implement !!
tadjacentNodes :: Node -> [Node] -> [Duration Node]
tadjacentNodes n ns = map (\x -> Duration (eliminate (adjT (n, x)), x)) filtered
                      where filtered = filter (\x -> adj(n,x)) ns

-- 3. Main body
{- For each node a in ns, if a is not already in p the function creates
   a new path (like in the previous problem) and computes its cost.
-}
-- !! to implement !
taddToEnd :: Duration Path -> [Duration Node] -> [Duration Path]
taddToEnd p ns | null as = [p]
               | otherwise = map (\x -> Duration(getDuration x + getDuration p,  (getValue p) ++ [(getValue x)]) ) as 
              where filteredList = filter ( `notElem` (getValue p)) (adjacentNodes (last (getValue p))   (map (\x -> getValue x)  ns))
                    as = (tadjacentNodes (last(getValue p)) filteredList)

-- .......
hCyclesCost :: Node -> [Duration Path]
hCyclesCost n = undefined

-- the main program                                    
tsp = minimum . hCyclesCost
