import System.IO  
import Control.Monad
import Data.List
import qualified Data.Map as Map

parseInt :: String -> Integer
parseInt = read

parseLine :: String -> [Integer]
parseLine line = map parseInt $ words line

parse :: [String] -> [[Integer]]
parse lines = map parseLine lines

isSmallIncrease :: (Integer, Integer) -> Bool
isSmallIncrease (f, s) = (s - f <= 3) && (s - f > 0)

isSmallDecrease :: (Integer, Integer) -> Bool
isSmallDecrease (f, s) = (f - s <= 3) && (f - s > 0)

isSafeStep :: ((Integer, Integer) -> Bool) -> [Integer] -> Bool
isSafeStep fct [x] = True
isSafeStep fct (f:s:xs) = (fct (f, s)) && (isSafeStep fct (s:xs))

isSafeStepDamp :: ((Integer, Integer) -> Bool) -> ([Integer], [Integer]) -> Bool
isSafeStepDamp fct ([x], a) = True
isSafeStepDamp fct (f:s:xs, p) = if fct (f, s) then (isSafeStepDamp fct ((s:xs), [f])) else ((isSafeStep fct (f:xs)) || (isSafeStep fct (p ++ s:xs)))

isSafe :: [Integer] -> Bool
isSafe x = (isSafeStep isSmallIncrease x) || (isSafeStep isSmallDecrease x)

isSafeDamp :: [Integer] -> Bool
isSafeDamp x = (isSafeStepDamp isSmallIncrease (x, [])) || (isSafeStepDamp isSmallDecrease (x, []))

q1 filename = do 
  lines <- fmap lines (readFile filename)
  print $ length $ filter isSafe $ parse lines

q2 filename = do 
  lines <- fmap lines (readFile filename)
  print $ length $ filter isSafeDamp $ parse lines
  
main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"