import System.IO  
import Control.Monad
import Data.List
import qualified Data.Map as Map

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)

zipPair :: ([Integer], [Integer]) -> [(Integer, Integer)]
zipPair (a1, a2) = zip a1 a2

parseInt :: String -> Integer
parseInt = read

parseLine :: String -> (Integer, Integer)
parseLine line = pairify $ map parseInt $ words line

parse :: [String] -> [(Integer, Integer)]
parse lines = map parseLine lines

sortColumns :: [(Integer, Integer)] -> [(Integer, Integer)]
sortColumns lines = zipPair (mapPair sort (unzip lines))

diff :: (Integer, Integer) -> Integer
diff (x, y) = abs (y - x)

computeDiff :: [(Integer, Integer)] -> Integer
computeDiff xs = sum $ map diff xs

listToMapCount :: Ord k => [k] -> Map.Map k Integer
listToMapCount [] = Map.empty
listToMapCount (k:xs) = Map.insertWith (+) k 1 (listToMapCount xs)

computeDist2 :: ([Integer], Map.Map Integer Integer) -> Integer
computeDist2 (a, m) = sum $ map (\(x) -> x * (Map.findWithDefault 0 x m)) a

applyToSnd :: (a -> b) -> (c, a) -> (c, b)
applyToSnd f (c, a) = (c, f a)

q1 filename = do 
  lines <- fmap lines (readFile filename)
  print $ computeDiff $ sortColumns $ parse lines

q2 filename = do 
  lines <- fmap lines (readFile filename)
  print $ computeDist2 $ applyToSnd listToMapCount $ unzip $ parse lines

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"
  
