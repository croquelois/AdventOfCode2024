import System.IO  
import Control.Monad
import qualified Data.Map as Map

type Pos = (Int, Int)
type Dir = (Int, Int)
type Grid = Map.Map Pos Char
allDir = [(0,1),(0,-1),(1,0),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]

parseInner :: String -> Pos -> Grid
parseInner [] _ = Map.empty
parseInner ('\n':str) (x,y) = parseInner str (0,y+1)
parseInner (c:str) (x,y) = Map.insert (x,y) c $ parseInner str (x+1,y)

parse :: String -> Grid
parse str = parseInner str (0, 0)

move :: Pos -> Dir -> Pos
move (x,y) (dx,dy) = (x+dx,y+dy)

next :: Char -> Char
next 'X' = 'M'
next 'M' = 'A'
next 'A' = 'S'
next _ = error "should not happen"

isXmas :: Grid -> Pos -> Char -> Dir -> Bool
isXmas g p c d = (Map.findWithDefault ' ' p g) == c && (c == 'S' || (isXmas g (move p d) (next c) d))

nbXmas :: Grid -> Pos -> Char -> Int -> Int
nbXmas g p 'X' a = a + (length $ filter (isXmas g p 'X') allDir)
nbXmas _ _ _ a = a

checkNewPos :: Grid -> Pos -> Dir -> Char -> Bool
checkNewPos g p d c = (Map.findWithDefault ' ' (move p d) g) == c

checkTwoPos :: Grid -> Pos -> (Dir, Dir) -> (Char, Char) -> Bool
checkTwoPos g p (d1, d2) (c1, c2) = ((checkNewPos g p d1 c1) && (checkNewPos g p d2 c2)) || ((checkNewPos g p d1 c2) && (checkNewPos g p d2 c1))

checkDiag1 :: Grid -> Pos -> (Char, Char) -> Bool
checkDiag1 g p cs = checkTwoPos g p ((-1,-1),(1,1)) cs

checkDiag2 :: Grid -> Pos -> (Char, Char) -> Bool
checkDiag2 g p cs = checkTwoPos g p ((-1,1),(1,-1)) cs

checkDiags :: Grid -> Pos -> (Char, Char) -> Bool
checkDiags g p cs = checkDiag1 g p cs && checkDiag2 g p cs 

nbCrossMas :: Grid -> Pos -> Char -> Int -> Int
nbCrossMas g p 'A' a | checkDiags g p ('M', 'S') = a + 1 
                     | otherwise = a
nbCrossMas _ _ _ a = a

q1 filename = do 
  content <- (readFile filename)
  let grid = parse content
  print $ Map.foldrWithKey (nbXmas grid) 0 grid

q2 filename = do 
  content <- (readFile filename)
  let grid = parse content
  print $ Map.foldrWithKey (nbCrossMas grid) 0 grid
  
main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"