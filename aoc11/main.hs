import System.CPUTime
import Control.Exception
import Text.Printf
import Control.DeepSeq
import Debug.Trace
import System.IO  
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Types and constants

-- Helpers

keepLeft :: (a, b) -> a
keepLeft (a, b) = a

keepRight :: (a, b) -> b
keepRight (a, b) = b

keepValues :: Map.Map k v -> [v]
keepValues = map keepRight . Map.toList

pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]

arrayToMapIn :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
arrayToMapIn n v m = Map.insertWith (+) v n m

arrayToMap :: Int -> Map.Map Int Int -> [Int] -> Map.Map Int Int
arrayToMap n m xs = foldr (arrayToMapIn n) m xs

applyNtimes :: (a -> a) -> Int -> a -> a
applyNtimes f 0 x = x
applyNtimes f n x = applyNtimes f (n-1) $ f x

-- Parsing

parse :: String -> [Int]
parse = map read . words

-- Computation

splitMiddle :: Int -> Maybe [Int]
splitMiddle x | r == 0 = Just $ map read $ pairToList$ splitAt n s
              | otherwise = Nothing
              where s = show x
                    (n,r) = divMod (length s) 2

oneStone :: Int -> [Int]
oneStone x | x == 0 = [1]
           | otherwise = maybe [(x * 2024)] id $ splitMiddle x

oneStep :: [Int] -> [Int]
oneStep [] = []
oneStep (x:xs) = (oneStone x) ++ (oneStep xs)

oneStone2 :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
oneStone2 x n m = arrayToMap n m $ oneStone x

oneStep2 :: Map.Map Int Int -> Map.Map Int Int
oneStep2 m = Map.foldrWithKey oneStone2 Map.empty m

-- Main

qMethod1 n filename = do 
  content <- readFile filename
  print $ length $ applyNtimes oneStep n $ parse content

qMethod2 n filename = do 
  content <- readFile filename
  print $ (sum . keepValues) $ applyNtimes oneStep2 n $ arrayToMap 1 Map.empty $ parse content

main = do 
  qMethod2 25 "test2.txt"
  qMethod2 25 "data.txt"
  qMethod2 75 "data.txt"
  
-- Debug

qVerbInner :: (NFData a) => (a -> a) -> (a -> Int) -> Int -> Int -> a -> IO ()
qVerbInner _ _ 0 _ _ = return() 
qVerbInner oneStep toInt n i arr = do 
  start <- getCPUTime
  arr2 <- evaluate $ force $ oneStep arr
  end <- getCPUTime
  printf "%02d %8d %d\n" i (div (end - start) 1000000000) $ toInt arr2
  hFlush stdout
  qVerbInner oneStep toInt (n-1) (i+1) arr2

qMethod1Verb n filename = do
  content <- readFile filename
  qVerbInner oneStep length n 1 $ parse content
  
qMethod2Verb n filename = do
  content <- readFile filename
  qVerbInner oneStep2 (sum . keepValues) n 1 $ arrayToMap 1 Map.empty $ parse content