test :: Int -> Int -> Bool
test n m = mod n 2 == 1 && mod m 2 == 1

stutter :: [Char] -> [Char]
stutter [] = []
stutter (x:xs) = x : x : stutter xs

compress :: [Char] -> [Char]
compress [] = []
compress [x] = [x]
compress (s:x:xs) = if x == s then x : compress xs else s : x : compress xs

zipSum :: [Int] -> [Int] -> [Int]
--zipSum [] [m] = [m]
--zipSum [n] [] = [n]
zipSum [] _ = []
zipSum _ [] = []
zipSum (n:ns) (m:ms) = n + m : zipSum ns ms

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion [] b = b
setUnion a [] = a
setUnion a@(n:ns) b@(m:ms) 
  | n == m = n : setUnion ns ms
  | n > m = m : setUnion a ms
  | n < m = n : setUnion ns b

setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection [] _ = []
setIntersection _ [] = []
setIntersection (n:ns) ms = if elem n ms then n : setIntersection ns ms else setIntersection ns ms

setDifference :: [Integer] -> [Integer] -> [Integer]
setDifference [] _ = []
setDifference _ [] = []
setDifference (n:ns) ms = if not (elem n ms) then n : setDifference ns ms else setDifference ns ms

setEqual :: [Integer] -> [Integer] -> Bool
setEqual [] [] = True
setEqual [] _ = False
setEqual _ [] = False
setEqual (n:ns) (m:ms) = if n == m then setEqual ns ms else False

dr :: Integer -> Int
dr x = if x < 10 then (fromIntegral x) else dr ((mod x 10) + toInteger (dr (div x 10)))
