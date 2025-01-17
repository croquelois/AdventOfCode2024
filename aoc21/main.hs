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

digicodeKeys = "A0123456789"
digicode1Step = Map.fromList [
  (('A', '0'), "<"),(('A', '3'), "^"),
  (('0', 'A'), ">"),(('0', '2'), "^"),
  (('1', '2'), ">"),(('1', '4'), "^"),
  (('2', '0'), "v"),(('2', '1'), "<"),(('2', '3'), ">"),(('2', '5'), "^"),
  (('3', 'A'), "v"),(('3', '2'), "<"),(('3', '6'), "^"),
  (('4', '1'), "v"),(('4', '5'), ">"),(('4', '7'), "^"),
  (('5', '2'), "v"),(('5', '4'), "<"),(('5', '6'), ">"),(('5', '8'), "^"),
  (('6', '3'), "v"),(('6', '5'), "<"),(('6', '9'), "^"),
  (('7', '4'), "v"),(('7', '8'), ">"),
  (('8', '5'), "v"),(('8', '7'), "<"),(('8', '9'), ">"),
  (('9', '6'), "v"),(('9', '8'), "<")]

dirpadKeys = "A<^v>"
dirpad1Step = Map.fromList [
  (('A','^'), "<"),(('A','>'), "v"),
  (('^','A'), ">"),(('^','v'), "v"),
  (('v','^'), "^"),(('v','>'), ">"),(('v','<'), "<"),
  (('>','A'), "^"),(('>','v'), "<"),
  (('<','v'), ">")]

-- Computation

type Open = Set.Set (Int, Char, String)
type Close = Map.Map Char (Int, Set.Set String)

shortestPathsFromWalk :: Map.Map (Char, Char) String -> Open -> Close -> Close
shortestPathsFromWalk steps o c | isNothing min = c
                                | rej = shortestPathsFromWalk steps ro c
                                | otherwise = shortestPathsFromWalk steps no nc
                                where min = Set.minView o
                                      ((ks, k, p), ro) = fromJust min
                                      (sz, ps) = Map.findWithDefault (9,Set.empty) k c
                                      rej = (ks > sz) || (Set.member p ps)
                                      rep = ks < sz
                                      up = Set.insert p ps
                                      nc = Map.insert k (ks, if rep then Set.singleton p else up) c
                                      n = Set.filter ((== k) . fst) $ Map.keysSet steps
                                      no = Set.union ro (Set.map (\k -> (ks+1, snd k,p ++ steps Map.! k)) n)
      
mapSnd :: Map.Map k (a,b) -> Map.Map k b
mapSnd = Map.map snd

fusionKey :: (Ord k1, Ord k2) => [(k2, Map.Map k1 a)] -> [Map.Map (k2, k1) a]
fusionKey = map (\(k2, m) -> Map.mapKeys (\k1 -> (k2, k1)) m)

transform :: (Ord k1, Ord k2) => [(k2, Map.Map k1 (a,b))] -> Map.Map (k2, k1) b
transform = mapSnd . Map.unions . fusionKey

removeSameKey :: Eq k => Map.Map (k,k) a -> Map.Map (k,k) a
removeSameKey = Map.filterWithKey (\(k1,k2) _ -> (k1 /= k2))

allShortestPath :: Map.Map (Char, Char) String -> String -> Map.Map (Char, Char) (Set.Set String)
allShortestPath steps keys = transform m
                      where m = map (\k -> (k, shortestPathsFromWalk steps (Set.singleton (0, k, "")) Map.empty)) keys

digicodePaths = allShortestPath digicode1Step digicodeKeys
dirpadPaths = allShortestPath dirpad1Step dirpadKeys

shortestPathsIn :: Map.Map (Char, Char) (Set.Set String) -> String -> [String]
shortestPathsIn allPaths ss | length ss <= 1 = [""]
                            | otherwise = map concat $ sequence [paths1, paths2]
                            where [k1, k2] = take 2 ss
                                  paths1 = map ((flip (++)) "A") $ Set.toList (allPaths Map.! (k1,k2))
                                  paths2 = shortestPathsIn allPaths (tail ss)

shortestPaths :: Map.Map (Char, Char) (Set.Set String) -> String -> [String]
shortestPaths allPaths str = shortestPathsIn allPaths ("A" ++ str)

shortestPathsArr :: Map.Map (Char, Char) (Set.Set String) -> [String] -> [String]
shortestPathsArr allPaths = concat . map (shortestPaths allPaths)

computeRn :: Int -> [String] -> Int
computeRn n keys | n == 1 = min
                 | otherwise = computeRn (n - 1) $ filter ((== min) . length) all
                 where all = shortestPathsArr dirpadPaths keys
                       min = minimum $ map length all

computeRnD :: Int -> String -> Int
computeRnD nBot code = computeRn nBot (filter ((== min) . length) all) 
                     where all = shortestPathsArr digicodePaths [code]
                           min = minimum $ map length all

scoreRnD :: Int -> String -> Int
scoreRnD nBot code = numPartOfCode * lengthOfBestPath
                   where numPartOfCode = read $ take 3 code
                         lengthOfBestPath = computeRnD nBot code

best = Map.fromList [
  ("", "A"),
  ("^", "<A>A"),
  (">", "vA^A"),
  ("v", "<vA^>A"),
  ("<", "v<<A>>^A"),
  ("^>", "<Av>A^A"),
  ("^<", "<Av<A>>^A"),
  ("v>", "<vA>A^A"),
  ("v<", "<vA<A>>^A"),
  ("v<<", "<vA<AA>>^A"),
  (">>", "vAA^A"),
  (">^", "vA<^A>A"),
  (">v", "vA<A^>A"),
  (">^>", "vA<^Av>A^A"),
  (">>^", "vAA<^A>A"),
  ("<<", "v<<AA>>^A"),
  ("<v", "v<<A>A^>A"),
  ("<^", "v<<A>^A>A"),
  ("<v<", "v<<A>A<A>>^A")]

find :: Ord k => k -> Map.Map k a -> a
find k m = case Map.lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x  -> x
    
computeNstr :: Int -> String -> String
computeNstr 0 code = code
computeNstr n code = computeNstr (n-1) $ concat $ map ((flip find) best) $ splitOn "A" $ init code

findBests :: String -> Int -> Set.Set (Int, String)
findBests keys n = Set.fromList $ map (\s -> (computeRn n [s], s)) $ shortestPaths dirpadPaths keys


computeN :: Int -> String -> Int
computeN n code = minimum $ map (length . (computeNstr (n - 1))) $ shortestPathsArr dirpadPaths $ shortestPaths digicodePaths code

scoreN :: Int -> String -> Int
scoreN nBot code = numPartOfCode * lengthOfBestPath
                   where numPartOfCode = read $ take 3 code
                         lengthOfBestPath = computeN nBot code

extendOne :: Map.Map String Int -> String -> Int
extendOne m k = sum $ map ((flip find) m) $ splitOn "A" $ init $ find k best

extend :: Map.Map String Int -> Map.Map String Int
extend m = Map.fromList $ map (\k -> (k, extendOne m k)) $ Map.keys m

extendN :: Int -> Map.Map String Int -> Map.Map String Int
extendN 0 m = m
extendN n m = extend $ (extendN (n - 1)) m

computeFromExt :: Map.Map String Int -> String -> Int
computeFromExt m code = minimum $ map (\k -> sum $ map ((flip find) m) $ splitOn "A" $ init k) $ shortestPathsArr dirpadPaths $ shortestPaths digicodePaths code

scoreNExt :: Int -> String -> Int
scoreNExt nBot code = numPartOfCode * lengthOfBestPath
                    where numPartOfCode = read $ take 3 code
                          lengthOfBestPath = computeFromExt ((extendN (nBot - 2)) $ Map.map length best) code

-- Main

q1 filename = do 
  content <- readFile filename
  print $ sum $ map (scoreN 2) $ lines content

q2 filename = do 
  content <- readFile filename
  print $ sum $ map (scoreNExt 25) $ lines content

main = do
  q1 "test1.txt"
  q1 "data.txt"
  q2 "data.txt"
