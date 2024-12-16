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

type Dir = (Int, Int)
type Pos = (Int, Int)
type Size = (Int, Int)
type Walls = Set.Set Pos
type Boxes = Set.Set Pos
type State = (Walls, Boxes, Pos)
left = (-1, 0)
right = (1,0)
up = (0,-1)
down = (0,1)

-- Helpers

keepLeft :: (a, b) -> a
keepLeft (a, b) = a

keepRight :: (a, b) -> b
keepRight (a, b) = b

keepMiddle :: (a, b, c) -> b
keepMiddle (a, b, c) = b

move :: Pos -> Dir -> Pos
move (x,y) (dx, dy) = (x+dx,y+dy)

-- Parsing

parseDir :: Char -> Dir
parseDir '>' = right
parseDir '<' = left
parseDir 'v' = down
parseDir '^' = up

parseAllDir :: String -> [Dir]
parseAllDir [] = []
parseAllDir (c:str) = (parseDir c):(parseAllDir str)

removeEol :: String -> String
removeEol str = concat $ lines str

parseInner :: Pos -> String -> (State, Size)
parseInner (x,y) [] = ((Set.empty, Set.empty, (-1, -1)), (x,y+1))
parseInner (x,y) ('\n':str) = parseInner (0,y+1) str
parseInner (x,y) ('#':str) = case parseInner (x+1,y) str of ((walls,boxes,start), size) -> ((Set.insert (x,y) walls,boxes,start), size)
parseInner (x,y) ('O':str) = case parseInner (x+1,y) str of ((walls,boxes,start), size) -> ((walls,Set.insert (x,y) boxes,start), size)
parseInner (x,y) ('@':str) = case parseInner (x+1,y) str of ((walls,boxes,start), size) -> ((walls,boxes,(x,y)), size)
parseInner (x,y) ('.':str) = parseInner (x+1,y) str
parseInner (x,y) _ = error "unexpected character in the grid"

parse2 :: [String] -> (State, [Dir], Size)
parse2 [grid, dir] = (state, parseAllDir $ removeEol dir, size)
                   where (state, size) = parseInner (0,0) grid

parse :: String -> (State, [Dir], Size)
parse str = parse2 $ splitOn "\n\n" str

-- Computation

isAgainstTheWall :: (Walls, Boxes) -> Dir -> Pos -> Bool
isAgainstTheWall (walls, boxes) d p | Set.member p walls = True
                                    | Set.member p boxes = isAgainstTheWall (walls, boxes) d (move p d)
                                    | otherwise = False

pushBoxesIn :: Walls -> Dir -> Boxes -> Pos -> Boxes
pushBoxesIn walls d boxes p | Set.member p boxes = Set.insert p $ pushBoxesIn walls d (Set.delete p boxes) (move p d)
                            | otherwise = Set.insert p boxes

pushBoxes :: State -> Dir -> State
pushBoxes (walls,boxes,pos) dir | isAgainstTheWall (walls, boxes) dir newPos = (walls,boxes,pos)
                                | otherwise = case pushBoxesIn walls dir (Set.delete newPos boxes) (move newPos dir) 
                                                of moved -> (walls,moved,newPos)
                                where newPos = (move pos dir)

oneStep :: State -> Dir -> State
oneStep (walls,boxes,pos) dir | isWall = (walls,boxes,pos)
                              | isBox = pushBoxes (walls,boxes,pos) dir
                              | otherwise = (walls,boxes,npos)
                              where npos = move pos dir
                                    isBox = Set.member npos boxes
                                    isWall = Set.member npos walls

allStep :: State -> [Dir] -> State
allStep state [] = state
allStep state (x:xs) = allStep (oneStep state x) xs

scoreOne :: Pos -> Int
scoreOne (x,y) = y*100+x

score :: State -> Int
score (_, boxes, _) = sum $ map scoreOne $ Set.toList boxes

-- Q2

move2 :: Pos -> Dir -> Pos
move2 (x,y) (dx, dy) = (x+2*dx,y+2*dy)

boxL :: Pos -> Pos
boxL (x,y) = (x-1,y)

boxR :: Pos -> Pos
boxR (x,y) = (x+1,y)

isH :: Dir -> Bool
isH (_,dy) = (dy == 0)

isV :: Dir -> Bool
isV (dx,_) = (dx == 0)

doubleWall :: Pos -> Walls -> Walls
doubleWall (x,y) w = Set.union w $ Set.fromList [(x*2,y), (x*2+1,y)]

doubleWalls :: Walls -> Walls
doubleWalls walls = Set.foldr doubleWall Set.empty walls

doubleBox :: Pos -> Boxes -> Boxes
doubleBox (x,y) w = Set.insert (x*2,y) w -- I don't insert the second side of the box

doubleBoxes :: Boxes -> Boxes
doubleBoxes boxes = Set.foldr doubleBox Set.empty boxes

doubleState :: State -> State
doubleState (walls, boxes, (x,y)) = (doubleWalls walls, doubleBoxes boxes, (x*2,y))

isAgainstTheWallV :: (Walls, Boxes) -> Dir -> Pos -> Bool
isAgainstTheWallV (walls, boxes) d p | Set.member p walls = True
                                     | Set.member p boxes = isAgainstTheWallV state d np || isAgainstTheWallV state d (move br d)
                                     | Set.member bl boxes = isAgainstTheWallV state d np || isAgainstTheWallV state d (move bl d)
                                     | otherwise = False
                                     where state = (walls, boxes)
                                           bl = boxL p
                                           br = boxR p
                                           np = move p d

pushBoxesVIn :: Walls -> Boxes -> Dir -> Pos -> Boxes
pushBoxesVIn walls boxes d p | Set.member p boxes = Set.insert p $ pushBoxesVIn walls (Set.delete p boxes) d (move p d)
                             | Set.member (boxL p) boxes && Set.member (boxR p) boxes = Set.insert p $ pushBoxesVIn walls (Set.delete (boxR p) (pushBoxesVIn walls (Set.delete (boxL p) boxes) d (move (boxL p) d))) d (move (boxR p) d)
                             | Set.member (boxR p) boxes = Set.insert p $ pushBoxesVIn walls (Set.delete (boxR p) boxes) d (move (boxR p) d)
                             | otherwise = Set.insert p boxes

isAgainstTheWallH :: (Walls, Boxes) -> Dir -> Pos -> Bool
isAgainstTheWallH (walls, boxes) d p | Set.member p walls = True
                                     | Set.member b boxes = isAgainstTheWallH (walls, boxes) d $ move2 p d
                                     | otherwise = False
                                     where b = if d == right then p else boxL p
                                   
pushBoxesHIn :: Walls -> Boxes -> Dir -> Pos -> Boxes
pushBoxesHIn walls boxes d p | Set.member b boxes = Set.insert nb $ pushBoxesHIn walls (Set.delete b boxes) d $ move2 p d
                             | otherwise = Set.insert nb boxes
                             where nb = if d == right then p else move p d
                                   b = if d == right then move p d else boxL $ move p d

pushBoxesH :: State -> Dir -> State
pushBoxesH (walls,boxes,pos) d | isAgainstTheWallH (walls, boxes) d newPos = (walls,boxes,pos)
                               | otherwise = case pushBoxesHIn walls (Set.delete box boxes) d (move newPos d) 
                                             of moved -> (walls,moved,newPos)
                               where newPos = move pos d
                                     box = if d == right then newPos else boxL newPos

pushBoxesV :: State -> Dir -> State
pushBoxesV (walls,boxes,pos) d | isAgainstTheWallV (walls, boxes) d newPos = (walls,boxes,pos)
                               | Set.member newPos boxes = case pushBoxesVIn walls (Set.delete newPos boxes) d (move newPos d)
                                                           of moved -> (walls,moved,newPos)
                               | Set.member newPosL boxes = case pushBoxesVIn walls (Set.delete newPosL boxes) d (move newPosL d)
                                                            of moved -> (walls,moved,newPos)
                               | otherwise = error ("unexpected" ++ show ((walls,boxes,pos), d))
                               where newPos = move pos d
                                     newPosL = boxL newPos


pushBoxes2 :: State -> Dir -> State
pushBoxes2 state dir | dir == right || dir == left = pushBoxesH state dir
                     | dir == up || dir == down = pushBoxesV state dir
                     | otherwise = error "unexpected direction"
                     
hasBox :: Pos -> Boxes -> Bool
hasBox (x,y) boxes = Set.member (x,y) boxes || Set.member (x-1,y) boxes

oneStep2 :: State -> Dir -> State
oneStep2 (walls,boxes,pos) dir | isWall = (walls,boxes,pos)
                               | isBox = pushBoxes2 (walls,boxes,pos) dir
                               | otherwise = (walls,boxes,npos)
                               where npos = move pos dir
                                     isBox = hasBox npos boxes
                                     isWall = Set.member npos walls

allStep2 :: State -> [Dir] -> State
allStep2 state [] = state
allStep2 state (x:xs) = allStep2 (oneStep2 state x) xs

-- Printing

grid2char :: State -> Pos -> Char
grid2char (w, b, s) p | p == s = '@'
                      | Set.member p w = '#'
                      | Set.member p b = 'O'
                      | otherwise = '.'

grid2charBig :: State -> Pos -> Char
grid2charBig (w, b, s) p | p == s = '@'
                         | Set.member p w = '#'
                         | Set.member p b = '['
                         | Set.member (boxL p) b = ']'
                         | otherwise = '.'

stringMapIn :: Bool -> Size -> State -> Pos -> String
stringMapIn isBig (w, h) state (x, y)
  | (x < 0 && y == 0) = ""
  | x < 0 = stringMapIn isBig (w, h) state (w-1, y-1) ++ "\n"
  | otherwise = stringMapIn isBig (w, h) state (x-1, y) ++ [(if isBig then grid2charBig else grid2char) state (x, y)]
  
stringMap :: Bool -> Size -> State -> String
stringMap isBig (w, h) state = stringMapIn isBig (w, h) state (w-1, h-1)

printState :: Bool -> Size -> State -> IO ()
printState isBig size state = putStrLn $ stringMap isBig size state


-- Main

test gridStr dirStr expStr = do
  let (state, dirs, size) = parse2 [gridStr, dirStr]
  let resStr = stringMap True size $ allStep2 state dirs
  putStrLn (if resStr == expStr then "\ESC[32mOK\ESC[0m " 
                                else ("\ESC[31mERROR\ESC[0m\n" ++ resStr ++ "\n\ESC[31mExpected\ESC[0m\n" ++ expStr))

dbg = do
  test ".@." ">" "..@"
  test ".@." "<" "@.."
  test "...\n.@." "^" ".@.\n..."
  test ".@.\n..." "v" "...\n.@."
  test ".@#" ">" ".@#"
  test ".@#" "<" "@.#"
  test ".#.\n.@." "^" ".#.\n.@."
  test ".@.\n.#." "v" ".@.\n.#."
  test "@O.O.##" ">" "@[][]##"
  test "@O...##" ">" ".@[].##"
  test "@O...##" ">>" "..@[]##"
  test "@O.O..##" ">" ".@[][]##"
  test "..@O.##" ">"  "..@[]##"
  test ".@O..##" ">>" "..@[]##"
  test "@O...##" ">>>" "..@[]##"
  test "@O.O..##" ">>" ".@[][]##"
  test "##O.O.@" "<" "##[][]@"
  test "##..O.@" "<" "##.[]@."
  test "##..O.@" "<<" "##[]@.."
  test "##.O.O.@" "<" "##[][]@."
  test "##O.@.." "<" "##[]@.."
  test "##O..@." "<<" "##[]@.."
  test "##.O.@." "<<" "##[]@.."
  test "##..O.@" "<<<" "##[]@.."
  test "##.O.O.@" "<<" "##[][]@."
  
  test ".#.\n.O.\n.@." "^" ".#.\n.[]\n.@."
  test ".#.\nO..\n.@." "^" ".#.\n[].\n.@."
  test "#..\n.O.\n.@." "^" "#[]\n.@.\n..."
  test "..#\nO..\n.@." "^" "[]#\n.@.\n..."
  test "....\nO.O.\n.O..\n..@." "^" "[][]\n.[].\n..@.\n...."
{-
  let (state, size) = parseInner (0,0) ".@O..##"
  print (state, size)
  let dirs = parseAllDir ">"
  printState True size state
  print state
  let newState = allStep2 state dirs
  printState True size newState
  print newState
  print $ parseInner (0,0) "..@O.##"
-}

q1 filename = do 
  content <- readFile filename
  let (state, dirs, size) = parse content
  print $ score $ allStep state dirs

q2 filename = do 
  content <- readFile filename
  let (state, dirs, size) = parse content
  print $ score $ allStep2 (doubleState state) dirs
  
q2print filename = do 
  content <- readFile filename
  let (state, dirs, (w,h)) = parse content
  let newState = allStep2 (doubleState state) dirs
  printState True (w*2,h) newState
  print $ score $ newState

main = do 
  q1 "test1.txt"
  q1 "test2.txt"
  q1 "data.txt"
  q2 "test2.txt"
  q2print "test3.txt"
  q2print "data.txt"

-- 1501897 is too low