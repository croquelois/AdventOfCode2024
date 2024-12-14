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

type Pos = (Int, Int)
type Size = (Int, Int)
type Speed = (Int, Int)
type Robot = (Pos, Speed)
type Zone = (Pos, Size)
allDir = [(0,-1), (1,0), (0,1), (-1,0), (-1,-1), (-1,1), (1,1), (1,-1)]

-- Helpers

keepLeft :: (a, b) -> a
keepLeft (a, b) = a

keepRight :: (a, b) -> b
keepRight (a, b) = b

applyRight :: (a -> b) -> (c, a) -> (c, b)
applyRight f (c, a) = (c, f a)

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f 0 x = x
applyNtimes f n x = applyNtimes f (n-1) $ f x

applyUntil :: (a -> a) -> (a -> Bool) -> a -> (a, Int)
applyUntil fctStep fctStop x | fctStop x = (x, 0)
                             | otherwise = applyRight (+ 1) $ applyUntil fctStep fctStop $ fctStep x

toMapCounterIn :: (Ord k) => k -> Map.Map k Int -> Map.Map k Int
toMapCounterIn k m = Map.insertWith (+) k 1 m

toMapCounter :: (Ord k) => [k] -> Map.Map k Int
toMapCounter robots = foldr toMapCounterIn Map.empty robots

-- Parsing

parseCoord :: String -> (Int, Int)
parseCoord str = case (map read $ splitOn "," str) of [x,y] -> (x,y)

parseLine :: String -> Robot
parseLine str = case (map parseCoord $ map (tail . tail) $ words str) of [p,s] -> (p, s)

parse :: String -> [Robot]
parse = map parseLine . lines

move :: Pos -> Speed -> Pos
move (x,y) (dx, dy) = (x+dx,y+dy)

-- Computation

wrap :: Size -> Pos -> Pos
wrap (w,h) (x,y) = (mod x w,mod y h)

moveOne :: Size -> Robot -> Robot
moveOne size (p, v) = (wrap size $ move p v, v) 

moveAll :: Size -> [Robot] -> [Robot]
moveAll size = map (moveOne size)

isIn :: Zone -> Robot -> Bool
isIn ((x,y),(w,h)) ((rx,ry),_) = rx >= x && ry >= y && rx < x+w && ry < y+h

quadrants :: Size -> [(Pos, Size)]
quadrants (w,h) = [((0,0),s2), ((w2+1,0),s2), ((0,h2+1),s2), ((w2+1,h2+1),s2)]
                  where w2 = div w 2
                        h2 = div h 2
                        s2 = (w2, h2)

keepInQuad :: [Robot] -> (Pos, Size) -> [Robot]
keepInQuad robots quad = filter (isIn quad) robots

score :: Size -> [Robot] -> Int
score size robots = product $ map length $ map (keepInQuad robots) $ quadrants size

checkInSet :: Set.Set Pos -> Pos -> Bool
checkInSet ps p = Set.member p ps

hasNeighbor :: Set.Set Pos -> Pos -> Bool
hasNeighbor ps p = not $ null $ filter (checkInSet ps) $ map (move p) allDir

proximityScoreSet :: Set.Set Pos -> Int
proximityScoreSet ps = length $ Set.filter (hasNeighbor ps) ps

proximityScore :: [Robot] -> Int
proximityScore robots = proximityScoreSet $ Set.fromList $ map keepLeft robots

-- Printing

grid2char :: Map.Map Pos Int -> Pos -> Char
grid2char grid p | n == 0 = '.'
                 | n > 9 = '#'
                 | otherwise = head $ show n
                 where n = Map.findWithDefault 0 p grid

stringMapIn :: Size -> Pos -> Map.Map Pos Int -> String
stringMapIn (w, h) (x, y) grid
  | (x < 0 && y == 0) = ""
  | x < 0 = stringMapIn (w, h) (w-1, y-1) grid ++ "\n"
  | otherwise = stringMapIn (w, h) (x-1, y) grid ++ [grid2char grid (x, y)]

stringMap :: Size -> Map.Map Pos Int -> String
stringMap (w, h) = stringMapIn (w, h) (w-1, h-1)

robotsToMap :: [Robot] -> Map.Map Pos Int
robotsToMap robots = toMapCounter $ map keepLeft robots

printMap :: Size -> [Robot] -> IO ()
printMap size robots = putStrLn $ stringMap size $ robotsToMap robots

-- Main

q1 size filename = do 
  content <- readFile filename
  print $ (score size) $ applyNtimes (moveAll size) 100 $ parse content

qVerbInner :: (NFData a) => (a -> a) -> (a -> Int) -> (a -> IO ()) -> Int -> Int -> Int -> a -> IO ()
qVerbInner _ _ _ 0 _ _ _ = return() 
qVerbInner oneStep toInt dbg n i m arr = do 
  start <- getCPUTime
  arr2 <- evaluate $ force $ oneStep arr
  s <- evaluate $ force $ toInt arr2
  end <- getCPUTime
  if s > m then printf "%02d %8d %d %d\n" (i+1) (div (end - start) 1000000000) (max m s) s else return ()
  if s > m then dbg arr2 else return ()
  hFlush stdout
  qVerbInner oneStep toInt dbg (n-1) (i+1) (max m s) arr2
  
qVerb :: (NFData a) => (a -> a) -> (a -> Int) -> (a -> IO ()) -> Int -> a -> IO ()
qVerb oneStep toInt dbg n arr = qVerbInner oneStep toInt dbg n 0 0 arr

q2Search size maxIter filename = do 
  content <- readFile filename
  qVerb (moveAll size) proximityScore (printMap size) maxIter $ parse content

q2Check size n filename = do 
  content <- readFile filename
  printMap size $ applyNtimes (moveAll size) n $ parse content
 
main = do 
  q1 (11,7) "test1.txt"
  q1 (101,103) "data.txt"
  q2Search (101,103) 10000 "data.txt"
--  q2Check (101,103) 7790 "data.txt"