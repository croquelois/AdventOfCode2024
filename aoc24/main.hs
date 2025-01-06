import System.IO  
import Data.List.Split
import Data.Ratio
import Data.Bits
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified System.Environment as SE
import Control.DeepSeq
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.Maybe

-- Types and constants

type Var = String
type Node = (Var, Op, Var, Var)
type Graph = Map.Map Var [Node]
type Memory = Map.Map Var Bool
data Op = XOR | OR | AND

-- Helpers

addToGraph :: Node -> Graph -> Graph
addToGraph n = (Map.insertWith (++) a [n]) . (Map.insertWith (++) b [n]) where (_, _, a, b) = n

addToDepGraph :: Node -> Graph -> Graph
addToDepGraph n = Map.insertWith (++) r [n] where (r, _, _, _) = n

inverseGraph :: Graph -> Graph
inverseGraph g = foldr addToDepGraph Map.empty $ concat $ Map.elems g

nodesToGraph :: [Node] -> Graph
nodesToGraph = foldr addToGraph Map.empty


opToStr :: Op -> String
opToStr AND = "AND"
opToStr XOR = "XOR"
opToStr OR = "OR"

instance Show Op where
    show o = opToStr o
    
toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

-- Parse

parseBool :: String -> Bool
parseBool "1" = True
parseBool "0" = False

parseMemLine :: String -> (Var, Bool)
parseMemLine s = case splitOn ": " s of [v, x] -> (v, parseBool x)

parseMem :: String -> Memory
parseMem s = Map.fromList $ map parseMemLine $ lines s

parseOp :: String -> Op
parseOp "AND" = AND
parseOp "XOR" = XOR
parseOp "OR" = OR

parseNode2 :: String -> Var -> Node
parseNode2 s r = case splitOn " " s of [a, o, b] -> (r, parseOp o, a, b)

parseNode :: String -> Node
parseNode s = case splitOn " -> " s of [o, r] -> parseNode2 o r

parseNodes :: String -> [Node]
parseNodes = map parseNode . lines

parseGraph :: String -> Graph
parseGraph = nodesToGraph . parseNodes

parse :: String -> ([Node], Memory)
parse s = case splitOn "\n\n" s of [m, g] -> (parseNodes g, parseMem m)

-- Computation

-- Close: Memory
-- Open: [Var]

canResolve :: Memory -> Node -> Bool
canResolve m (r,_,a,b) = and [Map.notMember r m, Map.member a m, Map.member b m]

getOp :: Op -> (Bool -> Bool -> Bool)
getOp AND = (&&)
getOp XOR = xor
getOp OR = (||)

applyOp :: Memory -> Node -> (Var, Bool)
applyOp m (r,o,a,b) = (r, (getOp o) (m Map.! a) (m Map.! b))

walk :: Graph -> Memory -> [Var] ->  Memory
walk g m [] = m
walk g m (v:vs) = walk g nm nvs
                where ops = filter (canResolve m) $ Map.findWithDefault [] v g
                      nvx = map (applyOp m) ops
                      nm = foldr (\(v,x) m -> Map.insert v x m) m nvx
                      nvs = vs ++ map fst nvx

compute :: (Graph, Memory) -> Memory
compute (g, m) = walk g m $ Map.keys m

buildReg :: Char -> Int -> String
buildReg c i = printf "%c%02d" c i

memToNumRec :: Memory -> Char -> Int -> Int
memToNumRec m c i = maybe 0 (\b -> (if b then (2 ^ i) else 0) + (memToNumRec m c (i + 1))) $ Map.lookup (buildReg c i) m

memToNum :: Char -> Memory -> Int
memToNum c m = memToNumRec m c 0

getNumReg :: Char -> Memory -> [(Int, String)]
getNumReg c m = map (\k -> (read $ drop 1 k,k)) $ filter (((==) c) . head) $ Map.keys m

getBit :: Int -> Int -> Bool
getBit v i = (v `shiftR` i) .&. 1 == 1

numToMem :: Char -> Int -> Memory -> Memory
numToMem c v m = foldr (\(s, b) m -> Map.insert s b m) m $ map (\(i, s) -> (s, getBit v i)) $ getNumReg c m

getDep :: Graph -> Var -> Set.Set Var
getDep g v = foldr (\(r,o,a,b) s -> Set.insert a $ Set.insert b s) Set.empty $ Map.findWithDefault [] v g

walkDep :: Graph -> Set.Set Var -> Set.Set Var -> Set.Set Var
walkDep g c o | isNothing min = c
              | otherwise = walkDep g nc no
              where min = Set.minView o
                    (n, ro) = fromJust min
                    nc = Set.insert n c
                    no = Set.union ro $ Set.filter ((flip Set.notMember) c) $ getDep g n

findAllDep :: Graph -> Set.Set Var -> Set.Set Var
findAllDep g v = walkDep (inverseGraph g) Set.empty v

findWrongBits :: Int -> Int -> [Int]
findWrongBits i1 i2 = map (\(i,_,_) -> i) $ filter (\(_,a,b) -> a /= b) $ zip3 [0..] b1 b2
                  where b1 = reverse $ toBin i1
                        b2 = reverse $ toBin i2

findWrongVars :: Int -> Int -> Set.Set Var
findWrongVars i1 i2 = Set.fromList $ map (buildReg 'z') $ findWrongBits i1 i2

switch :: (Var, Var) -> Var -> Var
switch (w1, w2) r | r == w1 = w2
                  | r == w2 = w1
                  | otherwise = r

switchTwoWires :: (Var, Var) -> [Node] -> [Node]
switchTwoWires w nodes = map (\(r,o,a,b) -> (switch w r,o,a,b)) nodes

switchWires :: [(Var, Var)] -> [Node] -> [Node]
switchWires ws nodes = foldr switchTwoWires nodes ws

-- Main

q1 filename = do 
  content <- readFile filename
  let (nodes, mem) = parse content
  print $ memToNum 'z' $ compute (nodesToGraph nodes, mem)

q2 filename = do 
  content <- readFile filename
  let (nodes, mem) = parse content
   -- wires found via an analysis in excel
  let wires = [("z07","swt"),("z13","pqc"),("z31","bgs"),("rjm","wsv")]
  let m = compute (nodesToGraph $ switchWires wires nodes, mem)
  let x = memToNum 'x' m
  let y = memToNum 'y' m
  let z = memToNum 'z' m
  print (x, y)
  print $ toBin z
  print $ toBin (x + y)
  print $ findWrongVars z (x + y)

test filename x y = do
  content <- readFile filename
  let (nodes, m) = parse content
  let g = nodesToGraph nodes
  let m2 = numToMem 'y' y $ numToMem 'x' x m
  let mem = compute (g, m2)
  let z = memToNum 'z' mem
  putStrLn (if z == x+y then ("\ESC[32mOK\ESC[0m " ++ (show (x,y,z,x+y)))
                        else ("\ESC[31mERROR\ESC[0m\n" ++ (show (x,y,z,x+y))))

main = do 
  q1 "test1.txt"
  q1 "test2.txt"
  q1 "data.txt"
  q2 "data.txt"
--  args <- SE.getArgs
--  print args 
--  test "data.txt" (read $ args !! 0) (read $ args !! 1)
  