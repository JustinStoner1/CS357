{-
Justin Stoner

JStoner
-}

--No other imports are allowed



module Homework2
 ( collatz, 
   haskellFileNames, 
   select, 
   prefixSum, 
   numbers, 
   Numeral, 
   makeLongInt, 
   evaluateLongInt, 
   changeRadixLongInt, 
   addLongInts, 
   mulLongInts
) where



import Data.List

--2.1

generateSequence :: Int -> [Int]
generateSequence s
  | s > 1 = (getNthTermOfSequence s) : (generateSequence (getNthTermOfSequence s))
  | s <= 1 = []

getNthTermOfSequence :: Int -> Int
getNthTermOfSequence n 
  | n == 1 = 1
  | even n = div n 2
  | odd n = 3*n+1

startSequence :: Int -> [Int]
startSequence n = n : generateSequence n

makeSequenceTuples :: [Int] -> [(Int,Int)]
makeSequenceTuples [] = []
makeSequenceTuples (x:xs) = (length (startSequence x), x) : makeSequenceTuples xs

quickSortTuples :: [(Int,Int)] -> [(Int,Int)]
quickSortTuples [] = []
quickSortTuples (x:xs) = quickSortTuples larger ++ [x] ++  quickSortTuples smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

removeLesserTuples :: [(Int,Int)] -> [(Int,Int)]
removeLesserTuples xs = takeWhile (\n -> fst n == fst (head xs)) xs

collatz :: [Int] -> Int
collatz xs = maximum $ map snd $ removeLesserTuples $ quickSortTuples $ makeSequenceTuples xs

--2.2

removeSpaces :: [Char] -> [Char]
removeSpaces [] = []
removeSpaces (x:xs) = if x == ' ' then removeSpaces xs else x : removeSpaces xs

haskellFileNames :: [String] -> [String]
haskellFileNames [] = []
haskellFileNames (x:xs)
  | (take 3 (reverse (removeSpaces x))) == "sh." = x : haskellFileNames xs
  | (take 4 (reverse x)) == "shl." = x : haskellFileNames xs
  | otherwise = haskellFileNames xs

--2.3

select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ [] [] = []
select func (x:xs) (n:ns)
  | func x = n : select func xs ns
  | otherwise = select func xs ns

--2.4

prefixSumProxy :: [Int] -> [Int]
prefixSumProxy [] = []
prefixSumProxy xs = (sum xs) : (prefixSumProxy (tail xs))

prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum xs = reverse (prefixSumProxy (reverse xs))

--2.5

--intListToString :: [Int] -> [Char]
--intListToString [] = []
--intListToString (x:xs) = show x ++ intListToString xs

numbers :: [Int] -> Int
numbers [] = 0
numbers (x:xs) = x * 10 ^ (length xs) + numbers xs

--2.6

type Numeral = (Int, [Int])
example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

--2.6 1

--baseTenToBaseX :: Integer -> Integer -> [Int]
--baseTenToBaseX 0 _ = []
--baseTenToBaseX n b = (fromInteger (mod n b)) : baseTenToBaseX (div n b) b

makeLongInt :: Integer -> Int -> Numeral
makeLongInt x r = (r,base10ToBaseXArith x r)--(r, splitNumber x)

--2.6 2

--baseXToBaseTen :: [Int] -> Int -> Int -> Integer
--baseXToBaseTen [] _ _ = 0
--baseXToBaseTen (x:xs) b p = toInteger (x*(b^p)) + baseXToBaseTen xs b (p+1)

evaluateLongInt :: Numeral -> Integer
evaluateLongInt n = baseXToBase10Arith (snd n) (fst n)--toInteger (numberify (snd n))

--2.6 3

--baseXToBaseX :: [Int] -> Int -> Int -> [Int]
--baseXToBaseX xs b1 b2 = baseTenToBaseX (baseXToBaseTen (reverse xs) b1 0) (toInteger b2)

changeRadixLongInt :: Numeral -> Int -> Numeral 
changeRadixLongInt n b = undefined

--2.6 4

addLongInts :: Numeral -> Numeral -> Numeral
addLongInts n1 n2
  | b1 == b2 = (b1, addTwoLongInts ((snd n1)) ((snd n2)) b1)
  | otherwise = error "not done yet"
    where b1 = fst n1
          b2 = fst n2

--2.6 5

mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts n1 n2
  | b1 == b2 = (b1, multTwoLongIntsOrder (reverse (snd n1)) (reverse (snd n2)) b1)
  | otherwise = error "not done yet"
    where b1 = fst n1
          b2 = fst n2

--Other

--Base Conversion

divionMod :: Int -> Int -> Int -> (Int,Int)
divionMod n d c
  | result > n = (c-1,result-n)
  | result == n = (c,0)
  | otherwise = divionMod n d (c+1)
    where result = d*c
{-
divionModList :: [Int] -> Int -> [Int] -> (Int,Int)
divionModList n d c
  | result > n = (c-1,result-n)
  | result == n = (c,0)
  | otherwise = divionModList n d (addTwoLongInts c [1])
    where result = reverse (multTwoLongIntsOrder [d] c 10)
-}
--baseTenToBaseX :: 

--ADD

addTwoLongInts :: [Int] -> [Int] -> Int -> [Int]
addTwoLongInts n1 n2 b = reverse (zipSumR (reverse n1) (reverse n2) 0 b)

zipSumR :: [Int] -> [Int] -> Int -> Int -> [Int]
zipSumR [] [] 0 _ = []
zipSumR [] [] r _ = r : []

zipSumR [] (m:ms) r b
  | x < b = x : zipSumR [] ms 0 b
  | x >= b = (mod x b) : zipSumR [] ms (div x b)  b
    where x = m + r
zipSumR (n:ns) [] r b
  | x < b = x : zipSumR ns [] 0 b
  | x >= b = (mod x b) : zipSumR ns [] (div x b) b
    where x = n + r

zipSumR (n:ns) (m:ms) r b
  | x < b = x : zipSumR ns ms 0 b
  | x >= b = (mod x b) : zipSumR ns ms (div x b) b
    where x = n + m + r

--MULT

multTwoLongIntsOrder :: [Int] -> [Int] -> Int -> [Int]
multTwoLongIntsOrder ns ms b
  | (length ns) > (length ms) = multTwoLongInts ms ns 0 b
  | (length ns) <= (length ms) = multTwoLongInts ns ms 0 b

multTwoLongInts :: [Int] -> [Int] -> Int -> Int -> [Int]
multTwoLongInts [] _ _ _ = []
multTwoLongInts (x:xs) l2 nZeros b = addTwoLongInts ( (reverse (multIntWithIntList x 0 l2 b)) ++ (replicate nZeros 0)) (multTwoLongInts xs l2 (nZeros+1) b) b

multIntWithIntList :: Int -> Int -> [Int] -> Int -> [Int]
multIntWithIntList _ _ [] _ = []
multIntWithIntList n c (x:xs) b = d : (multIntWithIntList n r xs b)
    where (r,d) = multTwoIntsWithCarry n x c b

multTwoIntsWithCarry :: Int -> Int -> Int -> Int -> (Int,Int)
multTwoIntsWithCarry n m c b
  | x < b = (0, x)
  | x >= b = ((div x b), (mod x b))
    where x = n * m + c

--2.6.1 and 2.6.2

base10ToBaseXArith :: Integer -> Int -> [Int]
base10ToBaseXArith 0 _ = []
base10ToBaseXArith x base = (base10ToBaseXArith (div x b) base) ++ (fromInteger(mod x b)):[]
  where b = toInteger base

baseXToBase10Arith :: [Int] -> Int -> Integer
baseXToBase10Arith [] _ = 0
baseXToBase10Arith (x:xs) base =  (toInteger(x) * toInteger(base) ^ toInteger(length xs)) + (baseXToBase10Arith xs base)

--Actual Garbage
{-
splitNumber :: Integer -> [Int]
splitNumber 0 = []
splitNumber x =  cutNumber (show x)

cutNumber :: [Char] -> [Int]
cutNumber [] = []
cutNumber (x:xs) = ((read (x:[])) :: Int) : (cutNumber xs)

numberify :: [Int] -> Int
numberify [] = 0
numberify (x:xs) = x * 10 ^ (length xs) + numberify xs

addTwoLongInts :: [Int] -> [Int] -> [Int]
addTwoLongInts n1 n2 = reverse (zipSumR (reverse n1) (reverse n2) 0)

zipSumR :: [Int] -> [Int] -> Int -> [Int]
zipSumR [] [] 0 = []
zipSumR [] [] r = r : []

zipSumR [] (m:ms) r
  | x < 10 = x : zipSumR [] ms 0
  | x >= 10 = (mod x 10) : zipSumR [] ms (div x 10) 
    where x = m + r
zipSumR (n:ns) [] r
  | x < 10 = x : zipSumR ns [] 0
  | x >= 10 = (mod x 10) : zipSumR ns [] (div x 10) 
    where x = n + r

zipSumR (n:ns) (m:ms) r
  | x < 10 = x : zipSumR ns ms 0
  | x >= 10 = (mod x 10) : zipSumR ns ms (div x 10) 
    where x = n + m + r
-}



