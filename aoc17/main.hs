import System.IO  
import Data.List.Split
import Data.Ratio
import Data.Bits
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Control.DeepSeq
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.Maybe

-- Types and constants
type Regs = (Int, Int, Int, Int, [Int]) --  A,B,C,Idx,Out
type Prog = [Int]

-- Helpers

getRegexSubmatches :: String -> String -> [String]
getRegexSubmatches regex str = case (str =~ regex :: (String, String, String, [String])) of (_,_,_,d) -> d

-- Parsing

registerRegex = "Register .: (.*)"
programRegex = "Program: (.*)"

parseRegister :: String -> Int
parseRegister str = read $ head $ getRegexSubmatches registerRegex str

parseRegisters :: String -> Regs
parseRegisters str = case map parseRegister $ lines str of [a,b,c] -> (a,b,c,0,[])

parseProgram :: String -> [Int]
parseProgram str = map read $ splitOn "," $ head $ getRegexSubmatches programRegex str

parse :: String -> (Regs, [Int])
parse str = case splitOn "\n\n" str of [registers, progam] -> (parseRegisters registers, parseProgram progam)

-- Computation

combo :: Regs -> Int -> Int
combo (a,b,c,_,_) v | v < 4 = v 
                    | v == 4 = a
                    | v == 5 = b
                    | v == 6 = c
                    | otherwise = error ("unexpected operand: " ++ (show v))

applyInstr :: Int -> Int -> Regs -> Regs
applyInstr 0 op (a,b,c,idx,out) = case div a $ (2 ^ (combo (a,b,c,idx,out) op)) of na -> (na,b,c,idx+2,out)
applyInstr 1 op (a,b,c,idx,out) = case xor b op of nb -> (a,nb,c,idx+2,out)
applyInstr 2 op (a,b,c,idx,out) = case mod (combo (a,b,c,idx,out) op) 8 of nb -> (a,nb,c,idx+2,out)
applyInstr 3 op (a,b,c,idx,out) = case if a == 0 then idx + 2 else op of nIdx -> (a,b,c,nIdx,out)
applyInstr 4 op (a,b,c,idx,out) = case xor b c of nb -> (a,nb,c,idx+2,out)
applyInstr 5 op (a,b,c,idx,out) = case mod (combo (a,b,c,idx,out) op) 8 of o -> (a,b,c,idx+2,out ++ [o])
applyInstr 6 op (a,b,c,idx,out) = case div a $ (2 ^ (combo (a,b,c,idx,out) op)) of nb -> (a,nb,c,idx+2,out)
applyInstr 7 op (a,b,c,idx,out) = case div a $ (2 ^ (combo (a,b,c,idx,out) op)) of nc -> (a,b,nc,idx+2,out)

processOne :: Prog -> Regs -> Regs
processOne prog (a,b,c,idx,out) = case take 2 $ drop idx $ prog
                                  of [i, o] -> applyInstr i o (a,b,c,idx,out)

processAll :: Prog -> Regs -> Regs
processAll prog (a,b,c,idx,out) | idx > ((length prog) - 2) = (a,b,c,idx,out)
                                | otherwise = processAll prog $ processOne prog $ (a,b,c,idx,out)

-- Q2

isPosGood :: Prog -> Regs -> Int -> Bool
isPosGood prog regs p | length out > p = (prog !! p) == (out !! p)
                      | otherwise = False
                      where out = case processAll prog regs of (_,_,_,_,o) -> o

computeA :: Int -> [Int] -> Int
computeA p xs = sum $ map (\(i,v) -> v * (8 ^ i)) $ zip [p..] xs

setA :: Regs -> Int -> [Int] -> Regs
setA (a,b,c,i,o) p xs = (computeA p xs,b,c,i,o)

possibles :: Prog -> Regs -> Int -> [Int] -> [Int]
possibles prog regs p xs = filter (\i -> isPosGood prog (setA regs p (i:xs)) p) [0,1,2,3,4,5,6,7]

searchIn :: Prog -> Regs -> Int -> [Int] -> [[Int]]
searchIn prog regs (-1) xs = [xs]
searchIn prog regs p xs = concat $ map (\n -> (searchIn prog regs (p - 1) (n:xs))) $ possibles prog regs p xs

search :: Prog -> Regs -> Int
search prog regs = minimum $ map (computeA 0) $ searchIn prog regs ((length prog) - 1) []

-- Main

q1 filename = do 
  content <- readFile filename
  let (regs, prog) = parse content
  print $ case processAll prog regs of (_,_,_,_,out) -> out

q2Seach filename = do
  content <- readFile filename
  let (regs, prog) = parse content
  print $ search prog regs

q2Test filename test = do 
  content <- readFile filename
  let ((a,b,c,i,o), prog) = parse content
  let regs = (test,b,c,i,o)
  print $ ("result: " ++ (show (case processAll prog regs of (_,_,_,_,out) -> out)))
  print $ ("expected: " ++ (show prog))

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2Seach "data.txt"
  q2Test "data.txt" 236539226447469
