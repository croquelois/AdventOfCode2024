import System.CPUTime
import Control.Exception
import Text.Printf
import Control.DeepSeq
import Debug.Trace
import System.IO  
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

-- Types and constants

type Pos = (Int, Int)
type Dir = (Int, Int)
type Corners = Set.Set Pos
type Field = Set.Set Pos
type Fields = [Field]
allDir = [(0,-1), (1,0), (0,1), (-1,0)]
allDiag = [(-1,-1), (-1,1), (1,1), (1,-1)]

-- Helpers

move :: Pos -> Dir -> Pos
move (x,y) (dx,dy) = (x+dx,y+dy)

updateRight :: (b -> c) -> (a, b) -> (a, c)
updateRight f (a, b) = (a,f b)

-- Parsing

addToField :: Pos -> Char -> Map.Map Char Field -> Map.Map Char Field
addToField p c = Map.insertWith Set.union c (Set.singleton p)

parseInner :: Pos -> String -> Map.Map Char Field
parseInner (x,y) [] = Map.empty
parseInner (x,y) ('\n':str) = parseInner (0,y+1) str
parseInner (x,y) (c:str) = addToField (x,y) c $ parseInner (x+1,y) str

parse :: String -> [Field]
parse = Map.elems . parseInner (0,0)

-- Computation

isNotIn :: Field -> Pos -> Bool
isNotIn field pos = Set.notMember pos field

isIn :: Field -> Pos -> Bool
isIn field pos = Set.member pos field

neighbor :: Pos -> [Pos]
neighbor pos = map (move pos) allDir

walkContigous :: Field -> Field -> [Pos] -> (Field, Field)
walkContigous field close [] = (field, close)
walkContigous field close (x:xs)
  | isIn close x = walkContigous field close xs
  | otherwise = walkContigous (Set.difference field $ Set.fromList ([x] ++ nexts)) (Set.insert x close) (xs ++ nexts)
  where nexts = filter (isIn field) $ neighbor x

contigousUpdateArgs :: Fields -> (Field, Field) -> (Field, Fields)
contigousUpdateArgs fields (todo, cont) = (todo, cont:fields)

contigousRec :: (Field, Fields) -> Fields
contigousRec (field, fields) 
  | Set.null field = fields
  | otherwise = contigousRec $ contigousUpdateArgs fields $ walkContigous field Set.empty $ List.singleton $ Set.findMin field

contigous :: Field -> Fields
contigous f = contigousRec (f, [])

contigousAll :: Fields -> Fields
contigousAll fields = concat $ map contigous fields

countOutside :: Field -> Pos -> Int
countOutside field pos = length $ filter (isNotIn field) $ neighbor pos

countBorder :: Field -> Int
countBorder field = Set.foldr ((+) . (countOutside field)) 0 field

price :: Field -> Int
price field = (length field) * (countBorder field)

-- Q2

doublePos :: Pos -> Pos
doublePos (x,y) = (2*x,2*y)

reducePos :: Pos -> Pos
reducePos (x,y) = ((div x 2),(div y 2))

cornerCode :: Field -> Pos -> [Bool]
cornerCode field pos = map (isIn field) $ map reducePos $ map (move pos) $ allDiag

nbCornerFromCode :: [Bool] -> Int
nbCornerFromCode [True , True , True , True ] = 0
nbCornerFromCode [False, False, False, False] = error "unexpected"
nbCornerFromCode [True , False, False, False] = 1
nbCornerFromCode [False, True , False, False] = 1
nbCornerFromCode [False, False, True , False] = 1
nbCornerFromCode [False, False, False, True ] = 1
nbCornerFromCode [True , True , False, False] = 0
nbCornerFromCode [False, True , True , False] = 0
nbCornerFromCode [False, False, True , True ] = 0
nbCornerFromCode [True , False, False, True ] = 0
nbCornerFromCode [True , True , True , False] = 1
nbCornerFromCode [False, True , True , True ] = 1
nbCornerFromCode [True , False, True , True ] = 1
nbCornerFromCode [True , True , False, True ] = 1
nbCornerFromCode [False, True , False, True ] = 2
nbCornerFromCode [True , False, True , False] = 2

cornersAround :: Pos -> Corners
cornersAround p = Set.fromList $ map (move $ doublePos p) allDiag

allCornersPossible :: Field -> Corners
allCornersPossible field = Set.unions $ Set.toList $ Set.map cornersAround field

countCorners :: Field -> Int
countCorners field = sum $ map nbCornerFromCode $ map (cornerCode field) $ Set.toList $ allCornersPossible field

price2 :: Field -> Int
price2 field = (length field) * (countCorners field)

-- Main

q1 filename = do 
  content <- readFile filename
  print $ sum $ map price $ contigousAll $ parse content

q2 filename = do 
  content <- readFile filename
  print $ sum $ map price2 $ contigousAll $ parse content

main = do 
  q1 "test1.txt"
  q1 "test2.txt"
  q1 "test3.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "test2.txt"
  q2 "data.txt"
  