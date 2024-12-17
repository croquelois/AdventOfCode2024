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

turnAnti :: Dir -> Dir
turnAnti d | d == left = down
           | d == down = right
           | d == right = up
           | d == up = left

turnClock :: Dir -> Dir
turnClock d | d == down = left
            | d == right = down
            | d == up = right
            | d == left = up

apply :: [(a -> b)] -> a -> [b]
apply fcts a = map (\f -> (f a)) fcts

-- Parsing

parseInner :: Pos -> String -> (Walls, Size)
parseInner (x,y) [] = (Set.empty, (x,y+1))
parseInner (x,y) ('\n':str) = parseInner (0,y+1) str
parseInner (x,y) ('#':str) = case parseInner (x+1,y) str of (walls, size) -> (Set.insert (x,y) walls, size)
parseInner (x,y) (_:str) = parseInner (x+1,y) str

parse :: String -> (Walls, Size)
parse str = parseInner (0,0) str

-- Computation

type OpenList = Set.Set (Int, Pos, Dir)
type CloseMap = Map.Map (Pos, Dir) Int

continue :: (Int, Pos, Dir) -> (Int, Pos, Dir)
continue (score, pos, dir) = (score + 1, move pos dir, dir)

turnLeft :: (Int, Pos, Dir) -> (Int, Pos, Dir)
turnLeft (score, pos, dir) = (score + 1000, pos, turnAnti dir)

turnRight :: (Int, Pos, Dir) -> (Int, Pos, Dir)
turnRight (score, pos, dir) = (score + 1000, pos, turnClock dir)

allNeighbors :: (Int, Pos, Dir) -> [(Int, Pos, Dir)]
allNeighbors pt = apply [continue, turnLeft, turnRight] pt

freeNeighbors :: Walls -> (Int, Pos, Dir) -> [(Int, Pos, Dir)]
freeNeighbors walls pt = filter (\(_, p, _) -> Set.notMember p walls) $ allNeighbors pt

isUseless :: CloseMap -> (Int, Pos, Dir) -> Bool
isUseless close (ns, p, d) = maybe False (<= ns) $ Map.lookup (p, d) close

removeUseless :: CloseMap -> [(Int, Pos, Dir)] -> [(Int, Pos, Dir)]
removeUseless close xs = filter (not . isUseless close) xs

addNeighbors :: Walls -> CloseMap -> OpenList -> (Int, Pos, Dir) -> OpenList
addNeighbors walls close open cur = (Set.union open $ Set.fromList $ removeUseless close $ freeNeighbors walls cur)


-- Close: map of cost to go to the position
-- Open: set of position to explore, with cost (cost first for ordering)
walkIn :: Walls -> CloseMap -> OpenList -> CloseMap
walkIn walls close open | isNothing cur = close
                        | otherwise = walkIn walls newClose newOpen
                        where (cur, rest) = maybe (Nothing, Set.empty) (\(a, b) -> (Just a, b)) $ Set.minView open
                              (curScore, pos, dir) = fromMaybe (999999, (-1,-1), (0,0)) cur
                              mOldScore = if isJust cur then Map.lookup (pos, dir) close else Nothing
                              stop = (isNothing cur) || maybe False (<= curScore) mOldScore
                              newClose = if stop then close else (Map.insert (pos, dir) curScore close)
                              newOpen = if stop then rest else addNeighbors walls newClose rest (fromJust cur) 

walk :: Walls -> Pos -> Pos -> Int
walk walls start end = minimum $ mapMaybe (flip Map.lookup close) allEnds
                     where close = walkIn walls Map.empty $ Set.singleton (0, start, right)
                           allEnds = map (\d -> (end, d)) allDir

-- Q2

revContinue :: (Int, Pos, Dir) -> (Int, Pos, Dir)
revContinue (score, pos, (dx,dy)) = (score - 1, move pos (-dx,-dy), (dx,dy))

revTurnLeft :: (Int, Pos, Dir) -> (Int, Pos, Dir)
revTurnLeft (score, pos, dir) = (score - 1000, pos, turnClock dir)

revTurnRight :: (Int, Pos, Dir) -> (Int, Pos, Dir)
revTurnRight (score, pos, dir) = (score - 1000, pos, turnAnti dir)

allPrev :: (Int, Pos, Dir) -> [(Int, Pos, Dir)]
allPrev pt = apply [revContinue, revTurnLeft, revTurnRight] pt

isPossible :: CloseMap -> (Int, Pos, Dir) -> Bool
isPossible close (s, p, d) = maybe False (== s) $ Map.lookup (p, d) close

keepPossible :: CloseMap -> [(Int, Pos, Dir)] -> [(Int, Pos, Dir)]
keepPossible close pts = filter (isPossible close) pts

walkBack :: CloseMap -> (Pos, Dir) -> (Int, Pos, Dir) -> Set.Set (Pos, Dir)
walkBack close start (s, p, d) | (p, d) == start = Set.singleton (p, d)
                               | otherwise = Set.unions $ map (Set.insert (p, d) . walkBack close start) $ keepPossible close $ allPrev (s, p, d)

enrichOne :: CloseMap -> (Pos, Dir) -> Maybe (Int, Pos, Dir)
enrichOne close (p,d) = maybe Nothing (\s -> Just (s,p,d)) $ Map.lookup (p,d) close

enrichAll :: CloseMap -> [(Pos, Dir)] -> [(Int, Pos, Dir)]
enrichAll close xs = mapMaybe (enrichOne close) xs

bestPlaces :: (Pos, Dir) -> Pos -> CloseMap -> Set.Set Pos 
bestPlaces start end close = Set.unions $ map ((Set.map keepLeft) . (walkBack close start)) allMinEnds
                           where allEnds = enrichAll close $ map (\d -> (end, d)) allDir
                                 m = minimum $ map (\(s,_,_) -> s) allEnds
                                 allMinEnds = filter (\(s,_,_) -> (s == m)) allEnds

-- Main

q1 filename = do 
  content <- readFile filename
  let (walls, (w, h)) = parse content
  let (start, end) = ((1, h-2), (w-2, 1))
  print $ walk walls start end
  
q2 filename = do 
  content <- readFile filename
  let (walls, (w, h)) = parse content
  let (start, end) = ((1, h-2), (w-2, 1))
  print $ length $ bestPlaces (start, right) end $ walkIn walls Map.empty $ Set.singleton (0, start, right)

main = do 
  q1 "test1.txt"
  q1 "test2.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "test2.txt"
  q2 "data.txt"
