import System.IO  
import Control.Monad
import Data.List.Split
import Data.Int
import Debug.Trace
import Data.Char

type I64 = Int64

-- Helpers

parseNum :: String -> I64
parseNum = read

keepLeft :: (a, b) -> a
keepLeft (a, b) = a

applyLeft :: (a -> c) -> (a, b) -> (c, b)
applyLeft f (a, b) = (f a, b)

applyRight :: (b -> c) -> (a, b) -> (a, c)
applyRight f (a, b) = (a,f b)

applyPair :: ((a -> c), (b -> d)) -> (a, b) -> (c, d)
applyPair (f1, f2) (a, b) = (f1 a, f2 b)

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

-- Parsing

parseRight :: String -> [I64]
parseRight = map parseNum . words . dropWhile isSpace

parseLine :: String -> (I64,[I64])
parseLine = applyPair (parseNum, parseRight) . pairify . splitOn ":"

parse :: [String] -> [(I64,[I64])]
parse = map parseLine

-- Computation
{- Original Q1
isPossibleInner :: I64 -> [I64] -> I64 -> Bool
isPossibleInner res [x] mAcc = res == mAcc * x || res == mAcc + x
isPossibleInner res (x:xs) mAcc = mAcc < res && (isPossibleInner res xs (mAcc + x) || isPossibleInner res xs (mAcc * x))

isPossible :: (I64,[I64]) -> Bool
isPossible (res, xs) = isPossibleInner res xs 0
-}

apply :: [I64 -> I64 -> I64] -> I64 -> I64 -> [I64]
apply [] _ _ = []
apply (f:fs) a b = (f a b):(apply fs a b)

isPossibleInner :: [I64 -> I64 -> I64] -> I64 -> [I64] -> I64 -> Bool
isPossibleInner ops res [x] mAcc = any (== res) $ apply ops mAcc x
isPossibleInner ops res (x:xs) mAcc = mAcc < res && (any (isPossibleInner ops res xs) $ apply ops mAcc x)

isPossible :: [I64 -> I64 -> I64] -> (I64,[I64]) -> Bool
isPossible ops (res, xs) = isPossibleInner ops res xs 0

concat :: I64 -> I64 -> I64
concat x1 x2 = parseNum (show x1 ++ show x2)

-- Main

q1 filename = do 
  content <- fmap lines (readFile filename)
  print $ sum $ map keepLeft $ filter (isPossible [(+), (*)]) $ parse content

q2 filename = do 
  content <- fmap lines (readFile filename)
  print $ sum $ map keepLeft $ filter (isPossible [(+), (*), Main.concat]) $ parse content

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"
