import Graphics.Image (readImageRGB, makeImageR, Pixel (PixelRGB, PixelY), cols, rows, VU (VU), writeImage, Image, RGB)
import Graphics.Image.Interface.Vector (VS (VS))
import Table (Point(Point), Cell (Blocked, Open, Start, End, Dirty, Occupied, Path), TableRow, TableState, cellState, genTableState, generateTable, fromImage, drawTable)
import Data.List (nub)
import GHC.Conc.IO (threadDelay)
import System.Random ( getStdGen, Random (randomRs) )
import GHC.IO.Handle.Types (BufferMode(BlockBuffering))

data Node = EmptyNode | Node Point FScore CameFrom deriving (Eq)
type FScore = Int
type CameFrom = Node

main :: IO ()
main = do
  maze <- readImageRGB VS "/Users/perkinsjonong/Public/Projects/ADAPH/AStar/maze.png"
  let stateFromImage = fromImage cellSize maze
      startAdded = cellState stateFromImage Start startingPoint
      initialState = cellState startAdded End endingPoint
  drawTable initialState
  evalNode initialState (Node startingPoint (hScore startingPoint) EmptyNode) []

evalNode :: TableState -> Node -> [Node] -> IO ()
evalNode _ EmptyNode _ = return ()
evalNode ts node@(Node p f _) s
  | h == 1 = do
    drawTable finalTableState
    generateImage $ cellState finalTableState Start startingPoint
    putStrLn "Endpoint found!"
  | otherwise = do
    drawTable updatedTableState
    generateImage $ cellState updatedTableState Start startingPoint
    threadDelay 125000
    evalNode updatedTableState (head newSet) (tail newSet)
  where h = hScore p
        n = neighbors node ts
        newSet = sortNodes (n ++ s)
        updatedTableState = updateCellStates (cellState ts Occupied p) Dirty n
        finalTableState = updateCellStates ts Path $ finalPath node

finalPath :: Node -> [Node]
finalPath EmptyNode = []
finalPath n@(Node p _ cameFrom) = n:finalPath cameFrom

updateCellStates :: TableState -> Cell -> [Node] -> TableState
updateCellStates ts cv = foldl (\acc (Node p _ _) -> cellState acc cv p) ts

sortNodes :: [Node] -> [Node]
sortNodes [] = []
sortNodes [EmptyNode] = []
sortNodes (EmptyNode:ns) = ns
sortNodes (nh@(Node _ fh _):ns) =
  let smallerSorted = sortNodes [ n | n@(Node _ f _) <- ns, f <= fh ]
      biggerSorted = sortNodes [ n | n@(Node _ f _) <- ns, f > fh ]
  in  smallerSorted ++ [nh] ++ biggerSorted

neighbors :: Node -> TableState -> [Node]
neighbors EmptyNode _ = []
neighbors n@(Node (Point x y) _ _) ts = [Node p (gScore p + (2*hScore p)) n | p <- ps, isValid p]
  where
    ps = [Point (x-1) y, Point (x+1) y, Point x (y-1), Point x (y+1)]
    isValid (Point x' y') = ((ts !! y') !! x') == Open

generateImage :: TableState -> IO ()
generateImage ts = do
  writeImage "/Users/perkinsjonong/Public/Projects/ADAPH/AStar/test.png" img
    where
      img = makeImageR VU (400, 400) (uncurry pixelVal) :: Image VU RGB Double
      pixelVal x y
        | Blocked == cs x y = PixelRGB 0.15294117647058825 0.1568627450980392 0.15294117647058825
        | Path == cs x y = PixelRGB 0.921 0.078 0.078
        | End == cs x y || Start == cs x y = PixelRGB 0.078 0.921 0.105
        | Occupied == cs x y = PixelRGB 0.078 0.192 0.921
        | otherwise = PixelRGB 1 1 1
      cs x y = (ts !! (x `div` 20)) !! (y `div` 20)

startingPoint :: Point
startingPoint = Point 1 1

endingPoint :: Point
endingPoint = Point 18 18

cellSize :: Int
cellSize = 20

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs(x2 - x1) + abs(y2 - y1)

hScore :: Point -> Int
hScore p = distance p endingPoint

gScore :: Point -> Int
gScore p = distance p startingPoint

-- start :: TableState -> IO Point
-- start ts = do
--   gen <- getStdGen
--   let rnds = randomRs(1,cellSize) gen
--       xs = filter odd rnds
--       ys = filter even rnds
--       p = head $ takeWhile isValid $ zip xs ys
--       isValid (x,y) = (ts !! x) !! y /= Blocked
--   return $ uncurry Point p
