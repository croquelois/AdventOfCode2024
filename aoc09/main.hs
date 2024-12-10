import System.IO  
import Control.Monad

-- Parsing

parseInner :: Bool -> Int -> String -> [(Int, (Bool, Int))]
parseInner _ _ [] = []
parseInner isUsed n (c:str) = [(read [c], (isUsed, n))] ++ parseInner (not isUsed) (if isUsed then n else n+1) str

parse :: String -> [(Int, (Bool, Int))]
parse = parseInner True  0

-- Computation

compact :: [(Int, (Bool, Int))] -> [(Int, (Bool, Int))]
compact [] = []
compact [x] = [x]
compact ((n1, (True , i1)):xs) | (not isUsed) = [(n1, (True , i1))] ++ (compact $ init xs)
                               | otherwise = [(n1, (True , i1))] ++ (compact xs)
                               where (n2, (isUsed, i2)) = last xs
compact ((n1, (False, i1)):xs) | (not isUsed) = compact $ ((n1, (False, i1)):(init xs))
                               | n1 == n2 = [(n1, (True, i2))] ++ (compact $ (init xs))
                               | n1 > n2 = [(n2, (True, i2))] ++ (compact $ ((n1-n2, (False, i1)):(init xs)))
                               | n1 < n2 = [(n1, (True, i2))] ++ (compact $ ((init xs) ++ [(n2-n1, (True, i2))]))
                               where (n2, (isUsed, i2)) = last xs

fullCompactToList :: [(Int, (Bool, Int))] -> [Int]
fullCompactToList [] = []
fullCompactToList ((0, _):xs) = fullCompactToList xs
fullCompactToList ((n, (b , i)):xs) = [i] ++ fullCompactToList ((n-1, (b, i)):xs)

mul :: (Int, Int) -> Int
mul (a, b) = a * b

checksum :: [Int] -> Int
checksum xs = sum $ map mul $ zip [0..] xs

-- Q2

forceEmptyTo0 :: [(Int, (Bool, Int))] -> [(Int, (Bool, Int))]
forceEmptyTo0 [] = []
forceEmptyTo0 ((n, (True, i)):xs) = ((n, (True, i)):(forceEmptyTo0 xs))
forceEmptyTo0 ((n, (False, i)):xs) = ((n, (False, 0)):(forceEmptyTo0 xs))

canFit :: [(Int, (Bool, Int))] -> (Int, Int) -> Bool
canFit [] _ = False
canFit ((n1, (True, i1)):xs) (n2, i2) = canFit xs (n2, i2) 
canFit ((n1, (False, i1)):xs) (n2, i2) = (n1 >= n2) || canFit xs (n2, i2)

insertIt :: [(Int, (Bool, Int))] -> (Int, Int) -> [(Int, (Bool, Int))]
insertIt [] _ = error "cannot fit"
insertIt ((n1, (True , i1)):xs) (n2, i2) = ((n1, (True, i1)):(insertIt xs (n2, i2)))
insertIt ((n1, (False, i1)):xs) (n2, i2) | (n1 > n2)  =  [(n2, (True , i2)), (n1-n2, (False, i1))] ++ xs ++ [(n2, (False, i1))]
                                         | (n1 == n2) =  [(n1, (True , i2))] ++ xs ++ [(n2, (False, i1))]
                                         | (n1 < n2)  =  [(n1, (False, i1))] ++ (insertIt xs (n2, i2))

compact2 :: [(Int, (Bool, Int))] -> [(Int, (Bool, Int))]
compact2 [] = []
compact2 [x] = [x]
compact2 ((n1, (True , i1)):xs) | (not isUsed) = [(n1, (True , i1))] ++ (compact2 $ init xs) ++ [(last xs)]
                                | otherwise = [(n1, (True , i1))] ++ (compact2 xs)
                                where (n2, (isUsed, i2)) = last xs
compact2 ((n1, (False, i1)):xs) | (not isUsed) = (compact2 $ ((n1, (False, i1)):(init xs))) ++ [(last xs)]
                                | n1 == n2 = [(n1, (True, i2))] ++ (compact2 $ init xs) ++ [(n2, (False, i1))]
                                | n1 > n2 = [(n2, (True, i2))] ++ (compact2 $ ((n1-n2, (False, i1)):(init xs))) ++ [(n2, (False, i1))]
                                | canFit (init xs) (n2, i2) = compact2 $ ((n1, (False, i1)):insertIt (init xs) (n2, i2))
                                | otherwise = (compact2 $ ((n1, (False, i1)):(init xs))) ++ [(last xs)]
                                where (n2, (isUsed, i2)) = last xs

-- Main

q1 filename = do 
  content <- readFile filename
  print $ checksum $ fullCompactToList $ compact $ parse content
  
q2 filename = do 
  content <- readFile filename
  print $ checksum $ fullCompactToList $ forceEmptyTo0 $ compact2 $ parse content

main = do 
  q1 "test1.txt"
  q1 "data.txt"
  q2 "test1.txt"
  q2 "data.txt"