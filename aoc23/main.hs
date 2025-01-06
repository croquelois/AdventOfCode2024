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

type Graph = Map.Map String (Set.Set String)

-- Helpers

addToGraph :: (String, String) -> Graph -> Graph
addToGraph (a, b) g = Map.insertWith Set.union a (Set.singleton b) $ Map.insertWith Set.union b (Set.singleton a) g

allNodes :: Graph -> Set.Set String
allNodes g = Map.keysSet g

next :: Graph -> String -> Set.Set String
next g n = Map.findWithDefault Set.empty n g 

isConnected :: Graph -> String -> String -> Bool
isConnected g n1 n2 = Set.member n2 $ next g n1

is1stChr :: Char -> String -> Bool
is1stChr c (x:xs) = x == c

-- Parse

parseLine :: String -> (String, String)
parseLine str = case splitOn "-" str of [a, b] -> (a, b)

parse :: String -> Graph
parse str = foldr addToGraph Map.empty $ map parseLine $ lines str

-- Computation

isInterConnected :: Graph -> String -> String -> String -> Bool
isInterConnected g n1 n2 n3 = and [isConnected g n1 n2, isConnected g n2 n3, isConnected g n1 n3]

walk2 :: Graph -> String -> String -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
walk2 g n1 n2 s = Set.union s $ Set.map (\n3 -> Set.fromList [n1, n2, n3]) $ Set.filter (isInterConnected g n1 n2) $ next g n2

walk3 :: Graph -> String -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
walk3 g n s = Set.foldr (walk2 g n) s $ next g n

filterLoop :: Set.Set (Set.Set String) -> Set.Set (Set.Set String)
filterLoop s = Set.filter (\sIn -> (length sIn == 3)) s

walk3All :: Graph -> Set.Set (Set.Set String)
walk3All g = filterLoop $ Set.foldr (walk3 g) Set.empty $ Set.filter (is1stChr 't') $ allNodes g

isConnectedToAll :: Graph -> Set.Set String -> String -> Bool
isConnectedToAll g s n = Set.null $ Set.filter (not . (isConnected g n)) s

walkInter :: Graph -> Map.Map String Bool -> Set.Set String -> Set.Set String
walkInter g c o | isNothing min = s
                | otherwise = walkInter g nc no
                  where min = Set.minView o
                        s = Map.keysSet $ Map.filter ((==) True) c
                        (n, ro) = fromJust min
                        ok = isConnectedToAll g (Map.keysSet $ Map.filter ((==) True) c) n
                        nc = Map.insert n ok c
                        no = if ok then Set.union ro $ Set.filter ((flip Map.notMember) c) $ next g n else ro

getInterconnected :: Graph -> String -> Set.Set String
getInterconnected g n = walkInter g Map.empty $ Set.singleton n

getAllInterconnected :: Graph -> Set.Set (Set.Set String)
getAllInterconnected g = Set.map (getInterconnected g) $ allNodes g

keepBiggest :: Set.Set String -> Set.Set String -> Set.Set String
keepBiggest s1 s2 = if length s1 > length s2 then s1 else s2

getBiggestSet :: Set.Set (Set.Set String) -> Set.Set String
getBiggestSet ss = Set.foldr keepBiggest Set.empty ss

setToPass :: Set.Set String -> String
setToPass s = List.intercalate "," $ Set.toList s 

-- Main

q1 filename = do 
  content <- readFile filename
  print $ length $ walk3All $ parse content

q2 filename = do 
  content <- readFile filename
  print $ setToPass $ getBiggestSet $ getAllInterconnected $ parse content

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"
