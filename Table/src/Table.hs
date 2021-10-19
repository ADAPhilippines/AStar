module Table
(
    TableRow,
    TableState,
    generateTable,
    genTableState,
    cellState,
    drawTable,
    Cell(..),
    Point(Point),
    fromImage
) where

import Graphics.Image (Pixel (PixelRGB), RGB, cols, displayImage, index, readImageRGB, rows, Image)
import Graphics.Image.Interface.Vector (VS (VS))  
import Data.List (transpose)

data Cell       =   Open | Blocked | Dirty | Occupied | Path | Start | End deriving (Eq)
data Point      =   Point Int Int deriving (Eq)
type RowState   =   [Cell]
type TableState =   [RowState]
type TableRow   =   String

generateRoof :: [Char]
generateRoof = "-----"

generateCell :: Cell -> [Char]
generateCell Open       =   "|   |"
generateCell Blocked    =   "| X |"
generateCell Dirty      =   "| * |"
generateCell Occupied   =   "| O |"
generateCell Path       =   "| @ |"
generateCell Start      =   "| S |"
generateCell End        =   "| E |"

generateRow :: RowState -> String
generateRow rs = cells ++ "\n" ++ flr
  where
    cells = unwords $ map generateCell rs
    flr = unwords $ replicate (length rs) generateRoof

generateTable :: TableState -> String
generateTable [] = ""
generateTable all@(h:_) = roof ++ "\n" ++ rows
  where
    rows = unlines $ map generateRow all
    roof = unwords $ replicate (length h) generateRoof

genTableState :: Int -> Int -> TableState
genTableState w h = replicate h $ replicate w Open

cellState :: TableState -> Cell -> Point -> TableState
cellState ts ctv (Point x y) = map (\i -> row i (ts !! i)) [0..length ts - 1]
  where
    row accY rs = map (\i -> consCell i accY (rs !! i)) [0..length rs - 1]
    consCell accX accY ccv
      | accX == x && accY == y = ctv
      | otherwise = ccv

cellMidPoints :: (Integral a) => a -> a -> [a]
cellMidPoints cd s = [i, (i + cd)..s]
  where i =  cd `div` 2

fromImage :: Int -> Image VS RGB Double -> TableState
fromImage cd img = [[cellState x y | x <- xmp] | y <- ymp]
  where
    w = rows img
    h = cols img
    xmp = cellMidPoints cd w
    ymp = cellMidPoints cd h
    cellState x y
      | index img(y, x) == whitePixel = Open
      | otherwise = Blocked

blackPixel :: Pixel RGB Double
blackPixel = PixelRGB 0.15294117647058825 0.1568627450980392 0.15294117647058825

whitePixel :: Pixel RGB Double
whitePixel = PixelRGB 1 1 1

drawTable :: TableState -> IO ()
drawTable ts = do
  putStr "\ESC[2J`"
  putStrLn $ generateTable ts