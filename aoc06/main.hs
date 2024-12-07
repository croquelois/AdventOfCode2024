import System.IO  
import Control.Monad
import qualified Data.Set as Set
import Data.List.Split
import Data.Maybe
import Debug.Trace

type Pos = (Int, Int)
type Dir = (Int, Int)
type Size = (Int, Int)
type Walls = Set.Set Pos
type Path = Set.Set Pos
up = (0,-1)
right = (1,0)
down = (0,1)
left = (-1,0)

turnRight :: Dir -> Dir
turnRight d | (d == up) = right
            | (d == right) = down
            | (d == down) = left
            | (d == left) = up
            | otherwise = error "unexpected direction"

updateLeft :: (a -> c) -> (a, b) -> (c, b)
updateLeft f (a, b) = (f a, b)

updateRight :: (b -> c) -> (a, b) -> (a, c)
updateRight f (a, b) = (a,f b)

parseInner :: String -> Pos -> (Pos, (Walls, Size))
parseInner [] (x,y) = ((-1, -1), (Set.empty, (x,y+1)))
parseInner ('\n':str) (x,y) = parseInner str (0,y+1)
parseInner ('#':str) (x,y) = updateRight (updateLeft (Set.insert (x,y))) $ parseInner str (x+1,y)
parseInner ('^':str) (x,y) = updateLeft (const (x, y)) $ parseInner str (x+1,y)
parseInner ('.':str) (x,y) = parseInner str (x+1,y)
parseInner _ _ = error "unexpected character"

parse :: String -> (Pos, (Walls, Size))
parse str = parseInner str (0, 0)

isOut :: Size -> Pos -> Bool
isOut (w,h) (x,y) = (x < 0 || y < 0 || x >= w || y >= h)

move :: Pos -> Dir -> Pos
move (p1, p2) (d1, d2) = (p1+d1, p2+d2)

-- p: current position
-- np: next potential position
-- d: current direction
walk :: (Walls, Size) -> Pos -> Pos -> Dir -> Path
walk (walls, size) p np d | isOut size np = Set.singleton p
                          | (Set.member np walls) = walk (walls, size) p (move p (turnRight d)) (turnRight d)
                          | otherwise = Set.insert p $ walk (walls, size) np (move np d) d

-- sp: start position
-- sd: start direction
-- p: current position
-- np: next potential position
-- d: current direction
-- vis: visited (position + direction)
-- nw: new additional wall
isWalkALoop :: (Walls, Size) -> Pos -> Pos -> Dir -> Set.Set (Pos, Dir) -> Pos -> Bool
isWalkALoop (walls, size) p np d vis nw | isOut size np = False -- Guard out
                                        | (Set.member (np, d) vis) = True -- Guard in a loop
                                        | np == nw || (Set.member np walls) = isWalkALoop (walls, size) p (move p (turnRight d)) (turnRight d) (Set.insert (p, d) vis) nw
                                        | otherwise = isWalkALoop (walls, size) np (move np d) d (Set.insert (p, d) vis) nw

q1 filename = do 
  content <- (readFile filename)
  let (start, (walls, size)) = parse content
  print $ length $ walk (walls, size) start (move start up) up

q2 filename = do 
  content <- (readFile filename)
  let (start, (walls, size)) = parse content
  let pwalls = walk (walls, size) start (move start up) up -- potential new wall positions
  print $ length $ Set.filter (isWalkALoop (walls, size) start (move start up) up (Set.singleton (start, up))) pwalls

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"

  
-- Useless for now

createWalls :: Dir -> Pos -> Pos -> Walls
createWalls dir pEnd pos | (pEnd == pos) = Set.singleton pos
                         | otherwise = Set.insert pos $ createWalls dir pEnd (move pos dir)

unionWalls :: [Walls] -> Walls
unionWalls [w] = w
unionWalls (w:ws) = Set.union w $ unionWalls ws

addOutline :: (Walls, Size) -> Walls
addOutline (walls, (w, h)) = let nothing = trace ("size=" ++ show (w,h)) 5
                                 top = createWalls (0,1) (-1,w) (-1,-1)
                                 bottom = createWalls (0,1) (h,w) (h,-1)
                                 left = createWalls (1,0) (h,-1) (-1,-1)
                                 right = createWalls (1,0) (h,w) (-1,w)
                             in unionWalls [walls, top, bottom, left, right]