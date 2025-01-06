import System.IO  
import Data.List.Split
import Data.Ratio
import Data.Bits
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

type Lock = (Int, Int, Int, Int, Int)
type Key = (Int, Int, Int, Int, Int)

-- Helpers

set :: Int -> a -> [a] -> [a]
set i v arr = (take i arr) ++ [v] ++ (drop (i + 1) arr)

-- Parse

parseLockRec :: String -> (Int, Int) -> [Int] -> [Int]
parseLockRec [] _ item = item
parseLockRec ('\n':xs) (_,h) item = parseLockRec xs (0,h+1) item
parseLockRec ('.':xs) (i,h) item = parseLockRec xs (i+1,h) item
parseLockRec ('#':xs) (i,h) item = parseLockRec xs (i+1,h) $ set i h item

parseKeyRec :: String -> (Int, Int) -> [Int] -> [Int]
parseKeyRec [] _ item = item
parseKeyRec ('\n':xs) (_,h) item = parseKeyRec xs (0,h+1) item
parseKeyRec ('#':xs) (i,h) item = parseKeyRec xs (i+1,h) item
parseKeyRec ('.':xs) (i,h) item = parseKeyRec xs (i+1,h) $ set i h item

parseLock :: String -> Lock
parseLock s = case parseLockRec s (0,0) [0,0,0,0,0] of [p0,p1,p2,p3,p4] -> (p0,p1,p2,p3,p4)

parseKey :: String -> Key
parseKey s = case parseKeyRec s (0,0) [0,0,0,0,0] of [p0,p1,p2,p3,p4] -> (5-p0,5-p1,5-p2,5-p3,5-p4)

isLock :: String -> Bool
isLock s = (take 5 s) == "#####"

route :: [String] -> ([String], [String])
route s = (filter isLock s, filter (not . isLock) s)

parse :: String -> ([Lock], [Key])
parse s = case route $ splitOn "\n\n" s of (locks, keys) -> (map parseLock locks, map parseKey keys)

-- Computation

doesFit :: Key -> Lock -> Bool
doesFit (k0,k1,k2,k3,k4) (l0,l1,l2,l3,l4) = (k0+l0<=5) && (k1+l1<=5) && (k2+l2<=5)  && (k3+l3<=5) && (k4+l4<=5) 

compute :: ([Lock], [Key]) -> Int
compute (locks, keys) = length $ filter (\[k, l] -> doesFit k l) $ sequence [keys, locks]

-- Main

q1 filename = do 
  content <- readFile filename
  print $ compute $ parse content

main = do 
  q1 "test1.txt"
  q1 "data.txt"