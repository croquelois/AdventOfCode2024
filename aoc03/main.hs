import System.IO  
import Control.Monad
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.List.Split

mulRegExp = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
doRegExp = "do\\(\\)"
dontRegExp = "don't\\(\\)"
allInstrRegExp = "(" ++ mulRegExp ++ "|" ++ doRegExp ++ "|" ++ dontRegExp ++ ")"
instrRegExp = "([a-z']+)\\(([0-9,]*)\\)"

parseInt :: String -> Integer
parseInt = read

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

getSubmatches :: (a,b,c,d) -> d
getSubmatches (_,_,_,d) = d

apply2nd :: (a -> b) -> [a] -> (a,b)
apply2nd f [x,y] = (x,f y)
apply2nd f _ = error "List must have exactly two elements"

getAllMulStr :: String -> [String]
getAllMulStr content = getAllTextMatches (content =~ mulRegExp) :: [String]

parseMulStr :: String -> (Integer, Integer)
parseMulStr str = pairify $ map parseInt $ getSubmatches (str =~ mulRegExp :: (String, String, String, [String]))

getAllMul :: String -> [(Integer, Integer)]
getAllMul content = map parseMulStr $ getAllMulStr content

getAllInstrStr :: String -> [String]
getAllInstrStr content = getAllTextMatches (content =~ allInstrRegExp) :: [String]

parseList :: String -> [Integer]
parseList [] = []
parseList str = map parseInt $ splitOn "," str

parseInstrStr :: String -> (String, [Integer])
parseInstrStr str = apply2nd parseList $ getSubmatches (str =~ instrRegExp :: (String, String, String, [String]))

getAllInstr :: String -> [(String, [Integer])]
getAllInstr content = map parseInstrStr $ getAllInstrStr content

mul :: (Integer, Integer) -> Integer
mul (x,y) = x*y

dontCompute :: [(String, [Integer])] -> Integer
dontCompute [] = 0
dontCompute (("do", []):xs) = doCompute xs
dontCompute (x:xs) = dontCompute xs

doCompute :: [(String, [Integer])] -> Integer
doCompute [] = 0
doCompute (("don't", []):xs) = dontCompute xs
doCompute (("mul", [x1,x2]):xs) = x1 * x2 + doCompute xs
doCompute (x:xs) = doCompute xs

q1 filename = do 
  content <- (readFile filename)
  print $ sum $ map mul $ getAllMul content

q2 filename = do 
  content <- (readFile filename)
  print $ doCompute $ getAllInstr content

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test2.txt"
  q2 "data.txt"