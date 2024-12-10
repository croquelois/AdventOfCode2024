import System.IO  
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Types and constants

type Pos = (Int, Int)
type Dir = (Int, Int)
type Grid = Map.Map Pos Int
up = (0,-1)
right = (1,0)
down = (0,1)
left = (-1,0)
allDir = [up, right, down, left]

-- Helpers

parseInt :: String -> Int
parseInt = read

move :: Pos -> Dir -> Pos
move (x,y) (dx,dy) = (x+dx,y+dy)

-- Parsing

parseInner :: String -> Pos -> Grid
parseInner [] (x,y) = Map.empty
parseInner ('\n':str) (x,y) = parseInner str (0,y+1)
parseInner (c:str) (x,y) = Map.insert (x,y) (parseInt [c]) $ parseInner str (x+1,y)

parse :: String -> Grid
parse str = parseInner str (0, 0)

-- Computation

getNext :: Pos -> Set.Set Pos
getNext pos = Set.fromList $ map (move pos) allDir

getIncNext :: Grid -> Int -> Pos -> Set.Set Pos
getIncNext grid c = Set.filter (posIs grid (c+1)) . getNext

getAt :: Grid -> Pos -> Int
getAt grid pos = Map.findWithDefault (-1) pos grid

posIs :: Grid -> Int -> Pos -> Bool
posIs grid n pos = getAt grid pos == n

get0 :: Grid -> Set.Set Pos
get0 = Map.keysSet . Map.filter (== 0)

-- Q1
followTo9 :: Grid -> Pos -> Set.Set Pos
followTo9 grid pos | c == 9 = Set.singleton pos
                   | otherwise = Set.unions $ Set.map (followTo9 grid) $ getIncNext grid c pos
                   where c = getAt grid pos


-- Q2

addPosToAllPath :: Pos -> Set.Set (Set.Set Pos) -> Set.Set (Set.Set Pos)
addPosToAllPath pos paths = Set.map (Set.insert pos) paths

allPathTo9 :: Grid -> Pos -> Set.Set (Set.Set Pos)
allPathTo9 grid pos | c == 9 = Set.singleton (Set.singleton pos)
                    | otherwise = Set.unions $ Set.map (addPosToAllPath pos . allPathTo9 grid) $ getIncNext grid c pos
                    where c = getAt grid pos

-- Main

q1 filename = do 
  content <- readFile filename
  let grid = parse content
  print $ sum $ map (length . followTo9 grid) $ Set.toList $ get0 grid
  
q2 filename = do 
  content <- readFile filename
  let grid = parse content
  print $ sum $ map (length . allPathTo9 grid) $ Set.toList $ get0 grid

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"