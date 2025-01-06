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

-- Computation

parseInt :: String -> Int
parseInt = read

prune :: Int -> Int
prune n = (mod n 16777216)

mix :: Int -> Int -> Int
mix n m = (xor n m)

mixAndPrune :: Int -> Int -> Int
mixAndPrune n m = prune $ mix n m

compute :: Int -> Int
compute n = mixAndPrune n2 (2048 * n2)
          where n1 = mixAndPrune n (64 * n)
                n2 = mixAndPrune n1 (div n1 32)

computeN :: Int -> Int -> Int
computeN 0 n = n
computeN i n = compute $ computeN (i-1) n

-- 1st: Sequence to look
-- 2nd: Previous Secret number
-- 3rd: Current Secret number
-- 4th: Max Iteration
-- 5th: Current Iteration
checkSeq :: [Int] -> Int -> Int -> Int -> Int -> Bool
checkSeq [] psec sec maxIter iter = (maxIter >= iter)
checkSeq (x:xs) psec sec maxIter iter | ((mod sec 10) - (mod psec 10)) /= x = False
                                      | otherwise = checkSeq xs sec (compute sec) maxIter (iter + 1)

-- 1st: Max Iteration
-- 2nd: Sequence to look
-- 3rd: Previous Secret number
-- 4th: Current Secret number
-- 5th: Current Iteration
seqGainIn :: Int -> [Int] -> Int -> Int -> Int -> Maybe Int
seqGainIn maxIter seq psec sec iter | isSeq = Just $ ((flip mod) 10) $  computeN 3 sec
                                    | iter == maxIter = Nothing
                                    | otherwise = seqGainIn maxIter seq sec (compute sec) (iter + 1)
                                    where isSeq = checkSeq seq psec sec maxIter iter

-- 1st: Max Iteration
-- 2nd: Sequence to look
-- 3rd: Secret number
seqGain :: Int -> [Int] -> Int -> Maybe Int
seqGain maxIter seq sec = seqGainIn maxIter seq sec (compute sec) 1


seqAllGain :: [Int] -> [Int] -> Int
seqAllGain secs seq = sum $ mapMaybe (seqGain 2000 seq) secs 

cross :: [Int] -> [[Int]] -> [[Int]]
cross [x] xs2 = map ((:) x) xs2
cross (x:xs1) xs2 = concat [map ((:) x) xs2, cross xs1 xs2]

generateAll :: [Int] -> Int -> [[Int]]
generateAll xs 1 = map (\x -> [x]) xs
generateAll xs n = cross xs $ generateAll xs (n - 1)

bruteForce :: [Int] -> Int
bruteForce secs = maximum $ map (seqAllGain secs) $ generateAll [-9..9] 4

createBatches :: Int -> [a] -> [[a]]
createBatches n [] = []
createBatches n arr = [take n arr] ++ createBatches n (drop n arr)

bruteForceVerbose :: [Int] -> Int -> [[[Int]]] -> IO ()
bruteForceVerbose _  max [] = do
  printf "Maximum %d\n" max
  hFlush stdout
bruteForceVerbose secs max (x:xs) = do 
  start <- getCPUTime
  newMax <- evaluate $ force $ maximum ([max] ++ (map (seqAllGain secs) x))
  end <- getCPUTime
  printf "Maximum %d, time: %8dms, batches left: %d\n" newMax (div (end - start) 1000000000) (length xs)
  hFlush stdout
  bruteForceVerbose secs newMax xs
  
-- Main

q1 filename = do 
  content <- readFile filename
  let codes = map parseInt $ lines content
  print $ sum $ map (computeN 2000) codes

q2 filename = do 
  content <- readFile filename
  let codes = map parseInt $ lines content
  print $ bruteForce codes

q2Verb filename = do 
  content <- readFile filename
  let codes = map parseInt $ lines content
  bruteForceVerbose codes 0 $ createBatches 1000 $ generateAll [-9..9] 4

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2Verb "test2.txt"
  q2Verb "data.txt"
