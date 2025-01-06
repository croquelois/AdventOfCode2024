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

-- Not available in my installed version
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

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

getBoxPos :: Pos -> Boxes -> Maybe Pos
getBoxPos (x, y) boxes | Set.member (x,y) boxes = Just (x,y)
                       | Set.member (x-1,y) boxes = Just (x-1,y)
                       | otherwise = Nothing
                       
isAgainstTheWallV :: (Walls, Boxes) -> Dir -> Pos -> Bool
isAgainstTheWallV (walls, boxes) d p | (Set.member nbl walls) || (Set.member nbr walls) = True
                                     | (isNothing nblp) && (isNothing nbrp) = False
                                     | nblp == nbrp = isAgainstTheWallV state d $ fromJust nblp
                                     | otherwise = or $ map (isAgainstTheWallV state d) $ catMaybes [nblp, nbrp]
                                     where state = (walls, boxes)
                                           bl = p
                                           br = boxR p
                                           nbl = move bl d
                                           nbr = move br d
                                           nblp = getBoxPos nbl boxes
                                           nbrp = getBoxPos nbr boxes

pushBoxesVIn :: Walls -> Boxes -> Dir -> Pos -> Boxes
pushBoxesVIn walls boxes d p | (isNothing blp) && (isNothing brp) = Set.insert p boxes
                             | blp == brp = Set.insert p $ pushBoxesVIn walls (Set.delete (fromJust blp) boxes) d (move (fromJust blp) d)
                             | otherwise = Set.insert p $ foldr (\p b -> pushBoxesVIn walls (Set.delete p b) d (move p d)) boxes $ catMaybes [blp, brp]
                             where bl = p
                                   br = boxR p
                                   blp = getBoxPos bl boxes
                                   brp = getBoxPos br boxes

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
pushBoxesV (walls,boxes,pos) d | isAgainstTheWallV (walls, boxes) d boxPos = (walls,boxes,pos)
                               | otherwise = case pushBoxesVIn walls (Set.delete boxPos boxes) d (move boxPos d)
                                             of moved -> (walls,moved,newPos)
                               where newPos = move pos d
                                     boxPos = fromJust $ getBoxPos newPos boxes

pushBoxes2 :: State -> Dir -> State
pushBoxes2 state dir | dir == right || dir == left = pushBoxesH state dir
                     | dir == up || dir == down = pushBoxesV state dir
                     | otherwise = error "unexpected direction"
                     
hasBox :: Pos -> Boxes -> Bool
hasBox p boxes = isJust $ getBoxPos p boxes

oneStep2 :: State -> Dir -> State
oneStep2 (walls,boxes,pos) dir | isWall = (walls,boxes,pos)
                               | isBox = pushBoxes2 (walls,boxes,pos) dir
                               | otherwise = (walls,boxes,npos)
                               where npos = move pos dir
                                     isBox = hasBox npos boxes
                                     isWall = Set.member npos walls

countBoxes :: State -> Int
countBoxes (walls,boxes,pos) = length boxes
  
control :: State -> State -> Bool
control s1 s2 = (countBoxes s1 == countBoxes s2)

allStep2 :: State -> [Dir] -> Int -> (State, Int)
allStep2 state [] n = (state, n)
allStep2 state (x:xs) n | control state newState = (newState, newN)
                        | otherwise = (state, n)
                        where (newState, newN) = allStep2 (oneStep2 state x) xs (n + 1)

nStep2 :: State -> [Dir] -> Int -> State
nStep2 state _ 0 = state
nStep2 state (x:xs) n = nStep2 (oneStep2 state x) xs (n - 1)
                        
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
  let resStr = stringMap True size $ fst $ allStep2 state dirs 0
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
  test "....\nO...\n.O..\n..@." "^" "[]..\n.[].\n..@.\n...."
  test "....\nO...\n..O.\n..@." "^" "....\n[][]\n..@.\n...."
  test "####\nO...\n.O..\n..@." "^" "####\n[]..\n.[].\n..@."
  test ".#..\n.O..\n..@." "^" ".#..\n.[].\n..@."

q1 filename = do 
  content <- readFile filename
  let (state, dirs, size) = parse content
  print $ score $ allStep state dirs

q2 filename = do 
  content <- readFile filename
  let (state, dirs, size) = parse content
  print $ score $ fst $ allStep2 (doubleState state) dirs 0

q2printLoop :: State -> Size -> [Dir] -> IO ()
q2printLoop _ _ [] = return ()
q2printLoop state size (x:xs) = do
  let newState = oneStep2 state x
  print x
  printState True size newState
  q2printLoop newState size xs
  

q2print filename = do 
  content <- readFile filename
  let (state, dirs, (w,h)) = parse content
  let dblState = (doubleState state)
  q2printLoop dblState (w*2,h) dirs
--  let (newState, rest) = allStep2 (doubleState state) dirs 0
--  printState True (w*2,h) newState
--  print $ score $ newState
--  print $ (countBoxes state, countBoxes newState)
--  printState True (w*2,h) $ nStep2 (doubleState state) dirs (rest - 1)
--  printState True (w*2,h) $ nStep2 (doubleState state) dirs (rest - 2)
--  printState True (w*2,h) $ nStep2 (doubleState state) dirs (rest - 3)

main = do 
--  dbg
  q1 "test1.txt"
  q1 "test2.txt"
  q1 "data.txt"
  q2 "test2.txt"
  q2 "test3.txt"
  q2 "data.txt"