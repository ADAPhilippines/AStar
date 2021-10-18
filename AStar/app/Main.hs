import Control.Concurrent
import Graphics.Image (readImageRGB)
import Graphics.Image.Interface.Vector (VS (VS))
import Table (Cell (Blocked), TableRow, cellState, genTableState, generateTable, fromImage)

main = do
  putStr "\ESC[2J"
  maze <- readImageRGB VS "/home/rawriclark/Projects/AStar/maze.png"
  let state = fromImage cellSize maze
  putStrLn $ generateTable state

cellSize :: Int
cellSize = 20



