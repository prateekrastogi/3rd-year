{--
Author: Alexander Savochkin
This is example of using STArray for Dijkstra algorithm implementation in Haskell.
--}
 
module Main where

import System.IO 
import Data.Char
import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import System.Environment
 
infinity = 1e+10
 
--Graph arc (directed edge). It points to other node of graph (int value is index of other node)
data GraphArc payload = GraphArc Int payload deriving Show                                   ---- pair of connected node and its weight
data GraphNode nodePayload arcPayload = GraphNode {nodePayload :: nodePayload, arcs :: [GraphArc arcPayload] } deriving Show      ---gets the node and list of its connected edges
data Graph ix nodePayload arcPayload = Graph (Array ix (GraphNode nodePayload arcPayload) ) deriving Show   ---our final graph
 
numberOfNodes (Graph nodesArray) = let (from, to) = bounds nodesArray in to - from + 1          ---finds the no. of nodes
getNeighbors (Graph nodesArray) nodesFrom = concat [  [(n,nodeTo,arcLength) | (GraphArc nodeTo arcLength) <- arcs (nodesArray!n) ] | n <- nodesFrom ]    --finds the neighbours of the nodes
 
--shortestPathDijkstra :: (Floating a, Ix ix) => Graph ix b a -> ix -> ix -> [ix] - compile error
-- Actual type is shortestPathDijkstra :: Graph Int b Double -> Int -> Int -> [Int] because of getNeighbors and 
shortestPathDijkstra graph from to = 
    let     
        markDijkstra marks [] currentStep = return ()
        markDijkstra marks activeNodes currentStep = do
            arcsToUpdate <- filterM ( \(arcNodeFrom, arcNodeTo, arcLength) -> do 
                (currentDistance, _, _) <- readArray marks arcNodeFrom
                (neighborDistance, neighborModifiedStep, cameFrom) <- readArray marks arcNodeTo
                if (currentDistance + arcLength < neighborDistance) then do
                    writeArray marks arcNodeTo (currentDistance + arcLength, currentStep, arcNodeFrom) 
                    return (neighborModifiedStep < currentStep)
                else 
                    return False)
                (getNeighbors graph activeNodes)
            markDijkstra marks [nodeToIndex | (_,nodeToIndex,_) <- arcsToUpdate ] (currentStep + 1)
            
        unwindDijkstraMarks accumulatedNodesList to marks = 
            if (\(_,second,_)->second) (marks!to) == 0 then to:accumulatedNodesList --we have found start node
            else 
                let (_,_,cameFrom) = marks!to in
                    unwindDijkstraMarks (to:accumulatedNodesList) cameFrom marks
 
        dijkstraMarks = runSTArray $ do
            marks <- newArray (1,numberOfNodes graph) (infinity, -1, -1)  --Initialize distances to all nodes as infinity
            writeArray marks from (0.0,0,-1)  --initialize distance to "from" node as 0
            markDijkstra marks [from] 1 --process dijkstra iterations recursively
            return marks
        
    in ( (unwindDijkstraMarks [] to dijkstraMarks), dijkstraMarks )
{--
Example of graph creation:
let g = Graph (array (1,2) [(1,GraphNode 1 [GraphArc 2 1.0]), (2,GraphNode 2 [GraphArc 1 1.0])])
 
      3-4 6
     /  |/ \
   1    5   7
     \ / \ /
      2   8
 
let g2 = Graph (array (1,7) [ (1, GraphNode () [GraphArc 2 1.0, GraphArc 3 1.0]),
                              (2, GraphNode () [GraphArc 5 1.0] ),
                              (3, GraphNode () [GraphArc 4 1.0] ),
                              (4, GraphNode () [GraphArc 5 1.0] ),
                              (5, GraphNode () [GraphArc 6 1.0, GraphArc 8 1.0] ),
                              (6, GraphNode () [GraphArc 7 1.0)]
                              (7, GraphNode () [] ),
                              (8, GraphNode () [GraphArc 7 1.0] )] )
--}
 
 
parseGraph lines = 
    let nonEmptyLine [] = False            ----checks if the line is empty or not
        nonEmptyLine s = not $ and $ map isSpace s
        parseArcs [] arcs = arcs
        parseArcs (to:weight:restWords) arcs = parseArcs restWords ( (GraphArc (read to) (read weight) ):arcs )  ----construct graph edges
        parseNode s =
            let (nodeNumber:arcsWords) = words s in
                (read nodeNumber::Int, GraphNode () (parseArcs arcsWords []) )
    in 
        let nodesList = map parseNode (filter nonEmptyLine lines)    ---list of nodes  in the graph
        in Graph (array (1, length nodesList) nodesList )          ----constructs a graph in which an array of length equal to total no. of nodes in the graph contains association list of edges for each node
 
{--
Loads graph from file:
Format:
 
    nodeid1 edge11 edge12 ...
    nodeid2 edge21 ...
    ...
Where edge is space separated pair: other node (id,weight) (see example below)
Example:
 
1 2 0.4 3 1.0
2 3 0.4
3
 
(1)  -0.4->-  (2)
 |             |
 v             v
1.0           0.4
 +-->-- (3)-<--+
 
--}
loadGraphFromFile :: String -> IO (Graph Int () Double)                        ----loads graph from the file
loadGraphFromFile fileName = do
    fileContent <- (readFile fileName)
    let fileLines = lines fileContent
    return $ parseGraph fileLines
 
 
main :: IO ()
main = do
   graph <- loadGraphFromFile "input.txt"
   print "Please enter the source node"
   x <- getLine 
   print "Please enter the destination node"
   y <- getLine
   print (shortestPathDijkstra graph (read x) (read y) )  
    --print graph
    ---putStrLn "Calculating shortest path"
    
