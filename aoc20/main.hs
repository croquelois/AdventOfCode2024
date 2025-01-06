import System.IO  
import Data.List.Split
import Data.Ratio
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.DeepSeq
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.Maybe

-- Types and constants

type Dir = (Int, Int)
type Pos = (Int, Int)
type Size = (Int, Int)
type Walls = Set.Set Pos
type Path = Set.Set Pos

left = (-1, 0)
right = (1,0)
up = (0,-1)
down = (0,1)
allDir = [left, right, up, down]

-- Helpers

move :: Pos -> Dir -> Pos
move (x,y) (dx, dy) = (x+dx,y+dy)

keepLeft :: (a,b) -> a
keepLeft (a,b) = a

apply :: [(a -> b)] -> a -> [b]
apply fcts a = map (\f -> (f a)) fcts

distance :: Pos -> Pos -> Int
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- Parsing

parseInner :: Pos -> String -> (Walls, Size, Pos, Pos)
parseInner (x,y) [] = (Set.empty, (x,y+1), (0,0), (0,0))
parseInner (x,y) ('\n':str) = parseInner (0,y+1) str
parseInner (x,y) ('#':str) = case parseInner (x+1,y) str of (ws, sz, s, e) -> (Set.insert (x,y) ws, sz, s, e)
parseInner (x,y) ('S':str) = case parseInner (x+1,y) str of (ws, sz, s, e) -> (ws, sz, (x,y), e)
parseInner (x,y) ('E':str) = case parseInner (x+1,y) str of (ws, sz, s, e) -> (ws, sz, s, (x,y))
parseInner (x,y) (_:str) = parseInner (x+1,y) str

parse :: String -> (Walls, Size, Pos, Pos)
parse str = parseInner (0,0) str

-- Computation

type OpenList = Set.Set (Int, Pos)
type CloseMap = Map.Map Pos Int

allNeighbors :: (Int, Pos) -> [(Int, Pos)]
allNeighbors (s, p) = map (\d -> (s + 1, move p d)) allDir

freeNeighbors :: Walls -> (Int, Pos) -> [(Int, Pos)]
freeNeighbors walls pt = filter (\(_, p) -> Set.notMember p walls) $ allNeighbors pt

isUseless :: CloseMap -> (Int, Pos) -> Bool
isUseless close (ns, p) = maybe False (<= ns) $ Map.lookup p close

removeUseless :: CloseMap -> [(Int, Pos)] -> [(Int, Pos)]
removeUseless close xs = filter (not . isUseless close) xs

addNeighbors :: Walls -> CloseMap -> OpenList -> (Int, Pos) -> OpenList
addNeighbors walls close open cur = (Set.union open $ Set.fromList $ removeUseless close $ freeNeighbors walls cur)


-- Close: map of cost to go to the position
-- Open: set of position to explore, with cost (cost first for ordering)
walkIn :: Walls -> CloseMap -> OpenList -> CloseMap
walkIn walls close open | isNothing cur = close
                        | otherwise = walkIn walls newClose newOpen
                        where (cur, rest) = maybe (Nothing, Set.empty) (\(a, b) -> (Just a, b)) $ Set.minView open
                              (curScore, pos) = fromMaybe (999999, (-1,-1)) cur
                              mOldScore = if isJust cur then Map.lookup pos close else Nothing
                              stop = (isNothing cur) || maybe False (<= curScore) mOldScore
                              newClose = if stop then close else (Map.insert pos curScore close)
                              newOpen = if stop then rest else addNeighbors walls newClose rest (fromJust cur) 

walk :: Walls -> Pos -> Pos -> Maybe Int
walk walls start end = Map.lookup end $! walkIn walls Map.empty $ Set.singleton (0, start)

notOnBorder :: Size -> Pos -> Bool
notOnBorder (w,h) (x,y) = not (x == 0 || y == 0 || x == (w-1) || y == (h-1))

wallsToRemove :: Walls -> Size -> [Pos]
wallsToRemove walls size = filter (notOnBorder size) $ Set.toList walls

checkCheats :: Walls -> Size -> Pos -> Pos -> [Int]
checkCheats walls size start end = map ((-) orig) $ filter (< orig) $ catMaybes possibles
                                 where orig = fromJust $ walk walls start end
                                       possibles = map (\w -> walk w start end) $ map (\w -> Set.delete w walls) $ wallsToRemove walls size

-- ss: score start
-- sc: score cheat
-- se: score end
-- ps: position start
-- pe: position end
-- tuple is (ss,sc,se,ps,pe)
checkCheats2b :: Int -> [(Int, Pos)] -> (Int, Pos) -> [(Int, Int, Int, Pos, Pos)]
checkCheats2b maxDist distEnd (ss, ps) = filter (\(_,d,_,_,_) -> d <= maxDist) $ allPossible
                                       where woStart = filter (\(_,pe) -> pe /= ps) distEnd
                                             allPossible = map (\(se,pe) -> (ss,(distance ps pe),se,ps,pe)) woStart

-- tuple is (score, position start, position end)
checkCheats2a :: Int -> CloseMap -> CloseMap -> [(Int, Int, Int, Pos, Pos)]
checkCheats2a maxDist distStart distEnd = concat $ map (checkCheats2b maxDist posEnd) posStart
                                        where reachable = Map.keys distStart
                                              posStart = map (\p -> (distStart Map.! p, p)) reachable
                                              posEnd = map (\p -> (distEnd Map.! p, p)) reachable

checkCheats2 :: Walls -> Pos -> Pos -> Int -> CloseMap -> CloseMap -> [Int]
checkCheats2 walls start end maxDist distStart distEnd = map ((-) orig) $ filter (< orig) uniques 
                                                       where orig = fromJust $ walk walls start end
                                                             possibles = checkCheats2a maxDist distStart distEnd
                                                             uniques = map (\(ss,sc,se,ps,pe) -> (ss+sc+se)) possibles

listToMapIn :: Int -> Map.Map Int Int -> Map.Map Int Int 
listToMapIn x m = Map.insertWith (+) x 1 m

listToMap :: [Int] -> Map.Map Int Int
listToMap xs = foldr listToMapIn Map.empty xs

keepOnlyAbove100 :: [Int] -> [Int]
keepOnlyAbove100 xs = filter (>= 100) xs

-- Main

q1Test filename = do 
  content <- readFile filename
  let (walls, size, start, end) = parse content
  print $ listToMap $ checkCheats walls size start end

q1WithQ2AlgoTest filename = do
  content <- readFile filename
  let (walls, size, start, end) = parse content
  let distEnd = walkIn walls Map.empty $ Set.singleton (0, end)
  let distStart = walkIn walls Map.empty $ Set.singleton (0, start)
  let dist = 2
  print $ listToMap $ checkCheats2 walls start end dist distStart distEnd 

q1 filename = do 
  content <- readFile filename
  let (walls, size, start, end) = parse content
  print $ length $ keepOnlyAbove100 $ checkCheats walls size start end

q1WithQ2Algo filename = do
  content <- readFile filename
  let (walls, size, start, end) = parse content
  let distEnd = walkIn walls Map.empty $ Set.singleton (0, end)
  let distStart = walkIn walls Map.empty $ Set.singleton (0, start)
  let dist = 2
  print $ length $ keepOnlyAbove100 $ checkCheats2 walls start end dist distStart distEnd 

q2Test filename = do 
  content <- readFile filename
  let (walls, size, start, end) = parse content
  let distEnd = walkIn walls Map.empty $ Set.singleton (0, end)
  let distStart = walkIn walls Map.empty $ Set.singleton (0, start)
  let orig = fromJust $ walk walls start end
  let dist = 20
  print $ listToMap $ checkCheats2 walls start end dist distStart distEnd 

q2 filename = do 
  content <- readFile filename
  let (walls, size, start, end) = parse content
  let distEnd = walkIn walls Map.empty $ Set.singleton (0, end)
  let distStart = walkIn walls Map.empty $ Set.singleton (0, start)
  let orig = fromJust $ walk walls start end
  let dist = 20
  print $ length $ keepOnlyAbove100 $ checkCheats2 walls start end dist distStart distEnd 

test filename = do
  content <- readFile filename
  let (walls, size, start, end) = parse content
  sTime <- getCPUTime
  res <- evaluate $ force $ walk walls start end
  eTime <- getCPUTime
  printf "time: %8dms\n" (div (eTime - sTime) 1000000000)
  hFlush stdout

main = do 
  q1Test "test1.txt"
  q1WithQ2AlgoTest "test1.txt"
--  q1 "data.txt" -- Slow
  q1WithQ2Algo "data.txt"
  
  q2Test "test1.txt"
  q2 "data.txt"