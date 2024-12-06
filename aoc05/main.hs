import System.IO  
import Control.Monad
import qualified Data.Map as Map
import Data.List.Split
import Debug.Trace

type Rule = (Int, Int)
type Rules = [Rule]
type RulesMap = Map.Map Int ([Int],[Int])
type Book = [Int]
type Books = [Book]

parseInt :: String -> Int
parseInt = read

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

parseRule :: String -> Rule
parseRule str = pairify $ map parseInt $ splitOn "|" str

parseRules :: [String] -> Rules
parseRules xs = map parseRule xs

parseBook :: String -> Book
parseBook str = map parseInt $ splitOn "," str

parseBooks :: [String] -> Books
parseBooks xs = map parseBook xs

parseInner :: (String, String) -> (Rules, Books)
parseInner (rulesStr, booksStr) = (parseRules $ lines rulesStr, parseBooks $ lines booksStr)

parse :: String -> (Rules, Books)
parse str = parseInner $ pairify $ splitOn "\n\n" str

checkRule :: Rule -> (Int, Int) -> Bool
checkRule (r1, r2) (p1, p2) = (r1 /= p2) || (r2 /= p1)

checkPair :: Rules -> (Int, Int) -> Bool
checkPair [] p = True
checkPair (r:rs) p = (checkRule r p) && (checkPair rs p)

checkPages :: Rules -> Int -> [Int] -> Bool
checkPages _ _ [] = True
checkPages rs p1 (p2:ps) = (checkPair rs (p1, p2)) && (checkPages rs p1 ps)

checkBook :: Rules -> Book -> Bool
checkBook _ [] = True
checkBook rs (p1:ps) = (checkPages rs p1 ps) && (checkBook rs ps)

checkBadBook :: Rules -> Book -> Bool
checkBadBook rs ps = not $ checkBook rs ps

filterBooks :: Rules -> Books -> Books
filterBooks rs bs = filter (checkBook rs) bs

repairFirstPage :: Rules -> Int -> Book -> Book -> Book
repairFirstPage rs p1 mid (p2:ps) | (checkPair rs (p1, p2)) = (repairFirstPage rs p1 (mid ++ [p2]) ps)
                                  | otherwise = ([p2] ++ mid ++ [p1] ++ ps)

repairBook :: Rules -> Book -> Book
repairBook _ [] = []
repairBook rs (p1:ps) | (checkPages rs p1 ps) = (p1:(repairBook rs ps))
                      | otherwise = (repairBook rs (repairFirstPage rs p1 [] ps))
                      
repairBooks :: Rules -> Books -> Books
repairBooks rs bs = map (repairBook rs) bs

middlePage :: Book -> Int
middlePage [x] = x
middlePage xs = middlePage $ (init . tail) xs

score :: Books -> Int
score [] = 0
score (b:bs) = (middlePage b) + (score bs)

q1 filename = do 
  content <- (readFile filename)
  let (rules, books) = parse content
  print $ score $ filterBooks rules books
  
q2 filename = do 
  content <- (readFile filename)
  let (rules, books) = parse content
  print $ score $ repairBooks rules $ filter (checkBadBook rules) books
  
main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"