import System.IO
import System.CPUTime
import Control.Exception
import Text.Printf
import Control.DeepSeq
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Debug.Trace

-- Types and constants

type Mem = Map.Map Int Int

-- Helpers

-- Not available in my installed version
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

-- Parsing

parse :: String -> ([String], [String])
parse str = case splitOn "\n\n" str of [patterns, designs] -> (words $ filter (/= ',') patterns, lines designs)

-- Computation

isOkPattern :: String -> String -> Bool
isOkPattern design pattern = pattern == take (length pattern) design

check :: [String] -> String -> Bool
check _ [] = True
check patterns design = any (check patterns) $ map (flip drop design) $ map length $ filter (isOkPattern design) patterns

sortPatterns :: [String] -> [String]
sortPatterns p = map snd $ Set.toList $ Set.fromList $ map (\s -> (length s, s)) p

pruneRec :: [String] -> [String] -> [String]
pruneRec [] p = p
pruneRec (x:xs) p = pruneRec xs (if check p x then p else (p ++ [x]))

prunePatterns :: [String] -> [String]
prunePatterns ps = pruneRec (sortPatterns ps) [] 

nbDiffWayIter :: Mem -> [String] -> String -> Int
nbDiffWayIter _ _ [] = 1
nbDiffWayIter mem patterns design = sum $ map ((flip (Map.findWithDefault 0) mem) . length) $ map (flip drop design) $ map length $ filter (isOkPattern design) patterns

nbDiffWayRec :: [String] -> String -> Mem
nbDiffWayRec _ [] = Map.singleton 0 1
nbDiffWayRec patterns design = Map.insert (length design) (nbDiffWayIter mem patterns design) mem
                               where mem = nbDiffWayRec patterns $ tail design
                               
nbDiffWay :: [String] -> String -> Int
nbDiffWay patterns design = Map.findWithDefault 0 (length design) $ nbDiffWayRec patterns design

-- Main

qVerbInner :: (String -> Bool) -> [String] -> IO ()
qVerbInner _ [] = return() 
qVerbInner fct (x:xs) = do 
  start <- getCPUTime
  arr2 <- evaluate $ force $ fct x
  end <- getCPUTime
  printf "%8d %s %s\n" (div (end - start) 1000000000) x (show arr2)
  hFlush stdout
  qVerbInner fct xs
  
q1 filename = do 
  content <- readFile filename
  let (patterns, designs) = parse content
  let prunedPatterns = prunePatterns patterns
  print $ length $ filter (check prunedPatterns) designs
  
q2 filename = do 
  content <- readFile filename
  let (patterns, designs) = parse content
  let prunedPatterns = prunePatterns patterns
  let okDesigns = filter (check prunedPatterns) designs
  print $ sum $ map (nbDiffWay patterns) okDesigns
  
main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"