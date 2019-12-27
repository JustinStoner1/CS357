{-
Justin Stoner
Jstoner
-}
--No Other Imports Are Allowed
import Data.List
--import Data.List.Split

--3.1 Lists And Trees (10pts)
data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Eq, Show)

balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs = NodeT (balance $ take halfLength xs) (balance $ drop halfLength xs)
        where 
            halfLength = div (length xs + 1) 2

--3.2 Simple Functions On Numbers (10pts)
goldbach :: Int -> [(Int,Int)]
goldbach x = if (even x) then checkSum x else []

--3.3 Higher-Order Functions (10pts)
church :: Int -> (c -> c) -> c -> c
church 0 f x = x
--church n f c = f $ church (n-1) f c
church 1 f x = f x
church 2 f x = f $ f x
church n f x = foldr (f .) (x) ((getFuncList (n) (useless)))

--3.4 Recursive Functions Over Lists (10pts)
type Set = [Int]

powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset xs = subsequences xs

--3.5 Lists And Strings (10pts)
example :: [[(Double, Double)]]
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

makeCommand :: [[(Double, Double)]] -> String
makeCommand xss = "%!PS-Adobe-3.0 EPSF-3.0\n"++
  "%%BoundingBox: "++(show (minimum xs))++" "++(show (minimum ys))++" "++(show (maximum xs))++" "++(show (maximum ys))++" "++"\n\n"++
  (getShapes xss)++
  "showpage\n"++"%%EOF"
  where xs = getXS $ concat xss
        ys = getYS $ concat xss
        

--3.6 Trees (25pts)
data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node t1 t2) = This : (map GoLeft (allpaths t1)) ++ (map GoRight (allpaths t2))

--3.7 Logic (25pts)
type Expr = [[Int]]

eval :: (Int -> Bool) -> Expr -> Bool
eval f [] = False
eval f xss = and (map or (flipNegativesOuter xss (map (map f) xss)))

--Similar to inits
satisfiable :: Expr -> Bool
satisfiable [] = False
satisfiable xss = evalTruthGroups xss (truthTable $ getRawComposition xss)

getRawComposition :: Expr -> [Int]
getRawComposition xss = removeDupes $ concat $ map (map (abs)) xss

evalTruthGroups :: Expr -> [[(Int,Bool)]] -> Bool
evalTruthGroups _ [] = False
evalTruthGroups xss (b:bs) = if (and $ evalTruthBlocks xss b) then True else (evalTruthGroups xss bs)

evalTruthBlocks :: Expr -> [(Int,Bool)] -> [Bool]
evalTruthBlocks [] _ = []
evalTruthBlocks (xs:xss) b = (evalTruthPairs xs b) : (evalTruthBlocks xss b)

evalTruthPairs :: [Int] -> [(Int,Bool)] -> Bool
evalTruthPairs [] _ = False
evalTruthPairs (x:xs) subTable = if result then True else (evalTruthPairs xs subTable)
  where absX = abs x
        true = elem (absX,True) subTable
        false = elem (absX,False) subTable
        result = if (x < 0) then false else true

truthPair :: Int -> [(Int, Bool)]
truthPair i = [ ((abs i),b) | b <- [True, False] ]

truthTable :: [Int] -> [[(Int,Bool)]]
truthTable [] = [[]]
truthTable (i:is) = [ p:tt | p <- truthPair i, tt <- truthTable is ]

removeDupes :: [Int] -> [Int]
removeDupes [] = []
removeDupes (n:xs) = n : removeDupes (filter (\x -> x /= n) xs)

--Helpers--------------------------------------------------------------------------------







--Balance
splitList :: [a] -> ([a],[a])
splitList [] = ([],[])
splitList xs = (take halfLength xs, drop halfLength xs)
    where 
      lengthV = length xs
      halfLength = div (lengthV+1) 2

--church
useless :: a -> a
useless x = x

getFuncList :: Int -> (a -> a) -> [a -> a]
getFuncList 0 _ = []
getFuncList n f = (f:[]) ++ (getFuncList (n-1) f)

--Goldbach
intsqrt :: Int -> Int
intsqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = and [not (mod n x == 0) | x <- [2..(intsqrt n)]]

listOfPrimes :: Int -> [Int]
listOfPrimes n = [x | x <- [2..n], isPrime x]

checkSum :: Int -> [(Int,Int)]
checkSum x = [(n,m) | n <- primes, m <- primes, n + m == x, n <= m]
  where primes = listOfPrimes x
  
--makeCommand
getXSS :: [[(Double, Double)]] -> [Double]
getXSS [] = []
getXSS (xs:xss) = (getXS xs) ++ (getXSS xss)

getXS :: [(Double, Double)] -> [Double]
getXS [] = []
getXS (x:xs) = (fst x) : (getXS xs)

getYSS :: [[(Double, Double)]] -> [Double]
getYSS [] = []
getYSS (xs:xss) = (getYS xs) ++ (getYSS xss)

getYS :: [(Double, Double)] -> [Double]
getYS [] = []
getYS (x:xs) = (snd x) : (getYS xs)

getMaxX :: [Double] -> Double
getMaxX xs = maximum xs

getMaxY :: [Double] -> Double
getMaxY ys = maximum ys

getMinX :: [Double] -> Double
getMinX xs = minimum xs

getMinY :: [Double] -> Double
getMinY ys = minimum ys

--getetBoundingBoxMax :: [[(Double, Double)]] -> Double
--getetBoundingBoxMax xss = maximum $ maximum $ maximum xss

--getetBoundingBoxMin :: [[(Double, Double)]] -> Double
--getetBoundingBoxMin xss = minimum $ minimum $ minimum xss

getShapes :: [[(Double, Double)]] -> String
getShapes [] = []
getShapes (x:xs) = (getLines x)++"\n"++(getShapes xs)

getLines :: [(Double, Double)] -> String
getLines [] = []
getLines (xs:xss) = (getTuplePartsAsString xs)++" moveTo\n"++getLinesProxy xss

getLinesProxy :: [(Double, Double)] -> String
getLinesProxy [] = "closepath\n"++"stroke\n"
getLinesProxy (x:xs) = (getTuplePartsAsString x)++" lineTo\n"++getLinesProxy xs

getTuplePartsAsString :: (Double, Double) -> String
getTuplePartsAsString tup = (show $ fst tup)++" "++(show $ snd tup)

--allPaths

--eval
flipNegativesOuter :: [[Int]] -> [[Bool]] -> [[Bool]]
flipNegativesOuter [] [] = []
flipNegativesOuter (is:iss) (bs:bss) = (flipNegativesInner is bs) : (flipNegativesOuter iss bss)

flipNegativesInner :: [Int] -> [Bool] -> [Bool]
flipNegativesInner [] [] = []
flipNegativesInner (i:is) (b:bs) = if (i >= 0) then (b) : (flipNegativesInner is bs) else (not b) : (flipNegativesInner is bs)

--satisfiable

  
--Actual Garbage
{-
powersetR       :: [a] -> [[a]]
powersetR []     = [[]]
powersetR (x:xs) = xss /\/ map (x:) xss
                where xss = powersetR xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

powersetR :: [a] -> [[a]]
powersetR [] = [[]]
powersetR (x:xs) = [x:ps | ps <- powersetR xs] ++ powersetR xs

--https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell

--[sort [n,m] | n <- [1,2,3], m <- [1,2,3]]

--removeDupes [sort [n,m] | n <- [1,2,3], m <- [1,2,3], m /= n] --pick2

--removeDupes  [sort [n,m,x] | n <- [1,2,3,4], m <- [1,2,3,4], x <- [1,2,3,4], n /= m, n /= x, m /= x] --pick3
pickK :: [Int] -> [[Int]]
pickK = undefined

removeDupes :: [[Int]] -> [[Int]]
removeDupes [] = []
removeDupes (n:xs) = n : removeDupes (filter (\x -> x /= n) xs)


height :: Tree x -> Int
height (LeafT _) = 1
height (NodeT x y) = 1 + (height x `max` height y)


isBalanced :: Tree a -> Bool
isBalanced (LeafT _) = True
isBalanced (NodeT l r) = 
    let diff = abs (height l - height r) in
    diff <= 1 && isBalanced l && isBalanced r
-}

testSatisfiable = and [
    satisfiable [[-1, 2, 4], [-2, -3]],
    satisfiable [[-1], [1,2]],
    not $ satisfiable [[-1], [1]],
    not $ satisfiable [[-1],[-1, 2, 4],[1]],
    satisfiable [[1, -1]],
    not $ satisfiable [[1], [-1]],
    satisfiable [[-1, 3], [1]],
    satisfiable [[1],[-1, 3]],
    satisfiable [[-1, 3], [3]],
    satisfiable [[4], [1, 3]],
    satisfiable [[4], [1, -3]],
    satisfiable [[-1, 3], [1, -3]],
    satisfiable [[1,-1], [1]],
    satisfiable [[5, 3, -1], [1, -3]],
    satisfiable [[1, 3, 5], [-2, -3]],
    satisfiable [[1, 3, 5], [2, 4], [6, 8]]
  ]
