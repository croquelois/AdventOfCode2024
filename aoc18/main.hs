import System.IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Debug.Trace

-- Types and constants

type Pos = (Int, Int)
type Dir = (Int, Int)
type Size = (Int, Int)
type Grid = Set.Set Pos
type Path = Set.Set Pos
type State = (Grid, Size, Pos)

type CloseMap = Map.Map Pos Path
type OpenList = Set.Set (Int, Pos, Path)

left = (-1, 0)
right = (1,0)
up = (0,-1)
down = (0,1)
allDir = [left, right, up, down]

-- Helpers

move :: Pos -> Dir -> Pos
move (x,y) (dx, dy) = (x+dx,y+dy)

-- Parsing

parsePos :: String -> Pos
parsePos str = case (map read $ splitOn "," str) of [x,y] -> (x,y)

parse :: String -> [Pos]
parse str = map parsePos $ lines str

-- Computation

buildGrid :: [Pos] -> Grid
buildGrid [] = Set.empty
buildGrid (x:xs) = Set.insert x $ buildGrid xs

freeNeighbors :: Grid -> (Int, Pos, Path) -> [Pos]
freeNeighbors grid (nb, pos, path) = filter (flip Set.notMember grid) $ map (move pos) allDir

computeDist :: Pos -> Pos -> Int
computeDist (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

buildNode :: Pos -> (Int, Pos, Path) -> [Pos] -> [(Int, Pos, Path)]
buildNode end (nb, pos, path) xs = map (\p -> ((length path)+1+(computeDist p end), p, Set.insert p path)) xs

isUseless :: CloseMap -> (Int, Pos, Path) -> Bool
isUseless close (_, pos, path) = maybe False (\b -> ((length path) >= (length b))) $ Map.lookup pos close

removeUseless :: CloseMap -> [(Int, Pos, Path)] -> [(Int, Pos, Path)]
removeUseless close xs = filter (not . isUseless close) xs

isOutside :: Size -> Pos -> Bool
isOutside (w,h) (x,y) = (x >= 0) && (x < w) && (y >= 0) && (y < h) 

removeOutside :: Size -> [Pos] -> [Pos]
removeOutside size xs = filter (isOutside size) xs

addNeighbors :: State -> CloseMap -> OpenList -> (Int, Pos, Path) -> OpenList
addNeighbors (grid, size, end) close open cur = Set.union open $ Set.fromList $ removeUseless close $ buildNode end cur $ removeOutside size $ freeNeighbors grid cur

walkIn :: State -> CloseMap -> OpenList -> Int
walkIn state close open | isNothing cur = -1
                        | pos == end = length path
                        | otherwise = walkIn state newClose newOpen
                        where (grid, size, end) = state
                              (cur, rest) = maybe (Nothing, Set.empty) (\(a, b) -> (Just a, b)) $ Set.minView open
                              (curScore, pos, path) = fromMaybe (999999, (-1,-1), Set.empty) cur
                              mOldPath = if isJust cur then Map.lookup pos close else Nothing
                              stop = (isNothing cur) || maybe False (\b -> ((length path) >= (length b))) mOldPath
                              newClose = if stop then close else (Map.insert pos path close)
                              newOpen = if stop then rest else addNeighbors state newClose rest (fromJust cur) 

walk :: State -> Pos -> Int
walk state start = walkIn state Map.empty $ Set.singleton (0, start, Set.singleton start)

bisection :: (Int -> Bool) -> Int -> Int -> Int -> Int
bisection fct try vOk vFail | try == 0 = -1
                            | vFail - vOk == 1 = vFail
                            | y = bisection fct (try - 1) vMid vFail
                            | otherwise = bisection fct (try - 1) vOk vMid
                            where vMid = (div (vOk + vFail) 2)
                                  y = fct vMid

canWalk :: Int -> [Pos] -> Int -> Bool
canWalk maxCoord objs n = (walk (grid, (maxCoord+1, maxCoord+1), (maxCoord, maxCoord)) (0,0) /= -1)
               where grid = buildGrid $ take n objs
-- Main

q1 maxCoord maxCorrupt filename = do 
  content <- readFile filename
  let objs = parse content
  let grid = buildGrid $ take maxCorrupt objs
  let state = (grid, (maxCoord+1, maxCoord+1), (maxCoord, maxCoord))
  print $ (walk state (0,0) - 1)

q2 maxCoord filename = do 
  content <- readFile filename
  let objs = parse content
  let vFail = bisection (canWalk maxCoord objs) 100 0 (length objs)
  print $ (objs !! (vFail - 1))

main = do 
  q1 6 12 "test1.txt"
  q1 70 1024 "data.txt"
  q2 6 "test1.txt"
  q2 70 "data.txt"