module Problem3 where
import System.IO

data Node = A | B | C | D | E | F deriving (Show,Eq,Ord)

type Path = [Node]


loadPaths = do
  ls <- fmap lines (readFile "file.txt")
  return ls

transPath :: (Node, Node) -> String
transPath (a, b) = show a ++ "," ++ show b

isAdjacent :: (Node, Node) -> [String] -> Bool
isAdjacent (a, b) paths = elem (transPath (a, b)) paths

adj :: (Node, Node) -> IO Bool 
adj(a, b) = do
           paths <- loadPaths
           return (isAdjacent (A, D) paths)



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

hCyclesAux :: Int -> [[Node]] -> [[Node]]
hCyclesAux 0 x = x
hCyclesAux i x = hCyclesAux (i-1) (concatMap (`addtoEnd` allNodes) x)
