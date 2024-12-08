import System.IO  
import Control.Monad
import Data.List.Split
import Data.Int
import Debug.Trace
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

type Pos = (Int, Int)
type Dir = (Int, Int)
type Size = (Int, Int)
type Antenas = Set.Set Pos

-- Helpers

keepLeft :: (a, b) -> a
keepLeft (a, b) = a

keepRight :: (a, b) -> b
keepRight (a, b) = b

keepValues :: Map.Map k v -> [v]
keepValues = map keepRight . Map.toList

applyLeft :: (a -> c) -> (a, b) -> (c, b)
applyLeft f (a, b) = (f a, b)

applyRight :: (b -> c) -> (a, b) -> (a, c)
applyRight f (a, b) = (a,f b)

applyPair :: ((a -> c), (b -> d)) -> (a, b) -> (c, d)
applyPair (f1, f2) (a, b) = (f1 a, f2 b)

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

move :: Pos -> Dir -> Pos
move (p1, p2) (d1, d2) = (p1+d1, p2+d2)

isOut :: Size -> Pos -> Bool
isOut (w,h) (x,y) = (x < 0 || y < 0 || x >= w || y >= h)

-- Parsing

addToAntenasMap :: Pos -> Char -> Map.Map Char Antenas -> Map.Map Char Antenas
addToAntenasMap p c = Map.insertWith Set.union c (Set.singleton p)

parseInner :: Pos -> String -> (Map.Map Char Antenas, Size)
parseInner (x,y) [] = (Map.empty, (x,y+1))
parseInner (x,y) ('\n':str) = parseInner (0,y+1) str
parseInner (x,y) ('.':str) = parseInner (x+1,y) str
parseInner (x,y) (c:str) = applyLeft (addToAntenasMap (x,y) c) $ parseInner (x+1,y) str

parse :: String -> (Map.Map Char Antenas, Size)
parse = parseInner (0,0)

-- Computation

listPairInner :: [a] -> [a] -> [(a,a)]
listPairInner [] [] = []
listPairInner [x] [] = []
listPairInner (x:y:xs) [] = listPairInner (y:xs) xs
listPairInner (x:xs) (y:ys) = [(x, y)] ++ listPairInner (x:xs) ys

listPair :: [a] -> [(a,a)]
listPair (x:xs) = listPairInner (x:xs) xs

allPairs :: Antenas -> Set.Set (Pos, Pos)
allPairs a = let xs = Set.toList a in Set.fromList $ listPair xs

getAntinode :: (Pos, Pos) -> Antenas
getAntinode ((x1, y1), (x2, y2)) = Set.fromList [(x1 + (x1 - x2), y1 + (y1 - y2)), (x2 + (x2 - x1), y2 + (y2 - y1))]

buildAntinodes :: Antenas -> Antenas
buildAntinodes xs = Set.unions $ Set.map getAntinode $ allPairs xs

buildAllAntinodes :: [Antenas] -> Antenas
buildAllAntinodes xs = Set.unions $ map buildAntinodes xs

removeOutside :: Size -> Antenas -> Antenas
removeOutside size = Set.filter (not . isOut size)

getAllAntinodes :: ([Antenas], Size) -> Antenas
getAllAntinodes (xs, size) = removeOutside size $ buildAllAntinodes xs

-- Q2

getAntinodeDir2 :: Size -> Dir -> Pos -> [Pos]
getAntinodeDir2 size dir pos | isOut size pos = [] 
                             | otherwise = (pos:(getAntinodeDir2 size dir (move pos dir)))

getAntinode2 :: Size -> (Pos, Pos) -> [Pos]
getAntinode2 size ((x1, y1), (x2, y2)) = getAntinodeDir2 size ((x1 - x2), (y1 - y2)) (x1, y1)  ++ getAntinodeDir2 size ((x2 - x1), (y2 - y1)) (x2, y2)

buildAntinodes2 :: Size -> Antenas -> Antenas
buildAntinodes2 size xs = Set.unions $ Set.map (Set.fromList . getAntinode2 size) $ allPairs xs

getAllAntinodes2 :: ([Antenas], Size) -> Antenas
getAllAntinodes2 (xs, size) = Set.unions $ map (buildAntinodes2 size) xs

-- Debug

reverseInner :: [(Char, [Pos])] -> Map.Map Pos Char
reverseInner [] = Map.empty
reverseInner ((c, []):ys) = reverseInner ys
reverseInner ((c, (x:xs)):ys) = Map.insert x c $ reverseInner ((c, xs):ys)

reverseMap :: Map.Map Char Antenas -> Map.Map Pos Char
reverseMap m = reverseInner $ map (applyRight Set.toList) $ Map.toList m

antenasChar :: Map.Map Pos Char -> Antenas -> Pos -> String
antenasChar node anti p | Map.member p node = [fromJust $ Map.lookup p node]
                        | Set.member p anti = "#"
                        | otherwise = "."

stringMap :: (Map.Map Pos Char, Size) -> Antenas -> Pos -> String
stringMap (xs, (w, h)) ys (x, y) 
  | (x < 0 && y == 0) = ""
  | x < 0 = stringMap (xs, (w, h)) ys (w-1, y-1) ++ "\n"
  | otherwise = stringMap (xs, (w, h)) ys (x-1, y) ++ antenasChar xs ys (x, y)

-- Main

q1 filename = do 
  content <- readFile filename
  print $ length $ getAllAntinodes $ applyLeft keepValues $ parse content

q2 filename = do 
  content <- readFile filename
  print $ length $ getAllAntinodes2 $ applyLeft keepValues $ parse content
  
main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"