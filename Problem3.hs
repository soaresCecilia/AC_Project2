module Problem3 where

import System.IO

data Node = A | B | C | D | E | F deriving (Show,Eq,Ord)

allNodes :: [Node]
allNodes = [A,B,C,D,E,F]

type Path = [Node]
type Graph = [(Node,Node)]

-- converts a char into a node
convertToNode :: Char -> Node
convertToNode c = case c  of 
                   'A' -> A
                   'B' -> B
                   'C' -> C
                   'D' -> D
                   'E' -> E
                   'F' -> F

-- string from file become a tuple of nodes
strToTup :: [Char] -> (Node, Node)
strToTup (h:comma:b:t) = (convertToNode h, convertToNode b) 


-- transforming a list of string into a Graph
transformIntoGraph :: [String] -> Graph 
transformIntoGraph lines = map (\x -> strToTup x) lines


-- reading a file into a IO Graph
loadPaths :: IO Graph
loadPaths = do
  ls <- fmap lines (readFile "file.txt")
  let lines = transformIntoGraph ls
  return lines


{- 
Returns a list of nodes that are adjacent of certain nodes
taking the Graph into account.
-}
adjacentNodes :: Node -> [Node] -> Graph -> [Node]
adjacentNodes n ns g = filter (\x -> elem (n,x) g) ns


{-
For each node a in ns, if a is not already in p the function
   creates a new path by adding to the end of p the element a.
-}
addtoEnd :: Path -> [Node] -> Graph -> [Path]
addtoEnd p ns g | null as = [p]
                | otherwise = map (\x -> p ++ [x]) as
                where as = filter (`notElem` p) (adjacentNodes (last p) ns g)

-- Computes all Hamiltonian cycles given a node and our Graph
hCycles :: Node -> Graph -> [Path]
hCycles n g = do
     { x <- hCyclesAux (length allNodes) [[n]] g;
       t <- filter (\l -> (elem(last l, n) g)  && length l == (length allNodes)) [x];
       map (\l -> l ++ [n]) [t] }

{- 
For all list of paths given we will compute all the possible paths 
without repeating nodes.
-}
hCyclesAux :: Int -> [Path] -> Graph -> [Path]
hCyclesAux 0 path graph = path
hCyclesAux i path graph = hCyclesAux (i-1) (concatMap (\x -> (addtoEnd x allNodes graph)) path) graph



-- Computes all Hamiltonian cycles starting from a given node
myCycles :: Node -> IO [Path]
myCycles n = do g <- loadPaths
                return (hCycles n g)
