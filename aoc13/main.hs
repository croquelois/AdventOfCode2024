import System.IO  
import Data.List.Split
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.Ratio
import qualified Data.List as List

-- Types and constants

type Coord = (Int, Int)
buttonRegex = "Button [A-Z]+: X\\+([0-9]+), Y\\+([0-9]+)"
prizeRegex = "Prize: X=([0-9]+), Y=([0-9]+)"

-- Helpers

getRegexSubmatchesIn :: (String, String, String, [String]) -> [String]
getRegexSubmatchesIn (_,_,_,d) = d

getRegexSubmatches :: String -> String -> [String]
getRegexSubmatches regex str = getRegexSubmatchesIn (str =~ regex :: (String, String, String, [String]))

triplify :: [a] -> (a, a, a)
triplify [a, b, c] = (a, b, c)
triplify _ = error "List must have exactly three elements"

pairify :: [a] -> (a,a)
pairify [x,y] = (x,y)
pairify _ = error "List must have exactly two elements"

applyTriple :: (a -> b, a -> b, a -> b) -> (a, a, a) -> (b, b, b)
applyTriple (f1, f2, f3) (a1, a2, a3) = (f1 a1, f2 a2, f3 a3)

toCoord :: [String] -> Coord
toCoord str = pairify $ map read str

-- Parsing

parseButton:: String -> Coord
parseButton str = toCoord $ getRegexSubmatches buttonRegex str

parsePrize:: String -> Coord
parsePrize str = toCoord $ getRegexSubmatches prizeRegex str

parseOne :: String -> (Coord, Coord, Coord)
parseOne str = applyTriple (parseButton, parseButton, parsePrize) $ triplify $ splitOn "\n" str

parse :: String -> [(Coord, Coord, Coord)]
parse str = map parseOne $ splitOn "\n\n" str

-- Computation

addToDest :: Int -> (Coord, Coord, Coord) -> (Coord, Coord, Coord)
addToDest n (a, b, (x,y)) = (a, b, (x+n,y+n))

computeOne :: (Coord, Coord, Coord) -> (Rational, Rational)
computeOne ((idx1,idy1),(idx2,idy2),(ix,iy)) =
  (n1, n2)
  where dx1 = fromIntegral idx1
        dy1 = fromIntegral idy1
        dx2 = fromIntegral idx2
        dy2 = fromIntegral idy2
        x = fromIntegral ix
        y = fromIntegral iy
        a = dy1/dx1
        n2 = (x*a - y)/(dx2*a - dy2)
        n1 = (x - n2 * dx2)/dx1

scoreOne :: Bool -> (Rational, Rational) -> Integer
scoreOne limit (n1, n2) | limit && (n1 > 100 || n2 > 100) = 0
                        | denominator n1 /= 1 || denominator n2 /= 1 = 0
                        | otherwise = numerator (n1 * 3 + n2 * 1)
-- Main

q1 filename = do 
  content <- readFile filename
  print $ sum $ map ((scoreOne True) . computeOne) $ parse content

q2 filename = do 
  content <- readFile filename
  print $ sum $ map ((scoreOne False) . computeOne) $ map (addToDest 10000000000000) $ parse content

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"
  