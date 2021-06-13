module Problem1 where

-- 1. Graph structure: nodes and adjacency matrix (i.e. the edges) 
data Node = A | B | C | D | E | F deriving (Show,Eq,Ord)

adj :: (Node,Node) -> Bool
adj p = case p of
  (A,B) -> True
  (A,C) -> True  
  (A,F) -> True 
  (B,A) -> True
  (B,C) -> True
  (B,E) -> True
  (B,F) -> True
  (C,A) -> True
  (C,B) -> True
  (C,D) -> True
  (D,C) -> True
  (D,E) -> True
  (E,B) -> True
  (E,D) -> True
  (E,F) -> True
  (F,A) -> True
  (F,B) -> True
  (F,E) -> True
  (_,_) -> False

type Path = [Node]

-- 2. Auxiliary functions
adjacentNodes :: Node -> [Node] -> [Node]
adjacentNodes n ns = filter (\x -> adj(n,x)) ns

allNodes :: [Node]
allNodes = [A,B,C,D,E,F]

choice :: ([a],[a]) -> [a]
choice = uncurry (++)

-- 3. Main body
{- For each node a in ns, if a is not already in p the function
   creates a new path by adding to the end of p the element a.
-}
-- !! To implement !!
addtoEnd :: Path -> [Node] -> [Path]
addtoEnd p ns | null as = [p]
              | otherwise = map (\x -> p ++ [x]) as
              where as = filter ( `notElem` p) (adjacentNodes (last p) ns) 



-- Computes all Hamiltonian cycles starting from a given node
-- !! To implement !!
hCycles :: Node -> [Path]
hCycles n = do
    { p <- addtoEnd [n] allNodes;
      x <- hCyclesAux (length allNodes) [p];
      t <- filter (\l -> adj(last l, n) && length l == (length allNodes)) [x];
      map (\l -> l ++ [n]) [t] }

hCyclesAux :: Int -> [Path] -> [Path]
hCyclesAux 0 x = x
hCyclesAux i x = hCyclesAux (i-1) (concatMap (`addtoEnd` allNodes) x)
