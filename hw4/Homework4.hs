{-
Justin Stoner
Jstoner
-}
--CALL THE PROVE FUNCTION TO SEE THE PROOFS
module Homework4 where

--No other imports allowed
import qualified Data.List as L
import Data.Char
--import Data.List
import System.IO

--4.1 Genome Lists (40pts)
insertions :: String -> [String]
insertions xs = [insertAt xs c i 0 | c <- "AGCT", i <- [0..(length xs)]]

deletions :: String -> [String]
deletions xs = [removeCharAt xs i 0 | i <- [0..((length xs)-1)]]

substitutions :: String -> [String]
substitutions xs = [subsituteAt xs c i 0 | c <- "AGCT", i <- [0..((length xs)-1)]]

transpositions :: String -> [String]
transpositions xs = [transposeAt xs i 0 | i <- [0..((length xs)-2)]]

--4.2 Sorting (20pts)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert n (x:xs) = if (n > x) then x : insert n xs else n : x : xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fileisort :: String -> String -> IO ()
fileisort fIn fOut = do
  input <- readFile fIn
  writeFile fOut (processInput input)
  
processInput :: String -> String
processInput input = unlines $ isort $ lines input

--4.3 Game Trees (40pts)
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field]

--CALL THE PROVE FUNCTION TO SEE THE PROOFS

strategyForRed :: Board -> Int
strategyForRed b
  | isEmpty b = 0
  | otherwise = findMove b $ gridToBoard (bestmove (boardToGrid b) (fieldToPlayer R))

strategyForGreen :: Board -> Int
strategyForGreen b
  | isEmpty b = 0
  | otherwise = findMove b $ gridToBoard (bestmove (boardToGrid b) (fieldToPlayer G))

--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined

--Helpers

--insertions

insertAt :: String -> Char -> Int -> Int -> String
insertAt [] c _ _ = [c]
insertAt (x:xs) c i s = if (i == s) then c:x:xs else x : insertAt xs c i (s+1)

--deletions

removeChar :: String -> Char -> String
removeChar [] _ = []
removeChar (x:xs) c = if (x == c) then xs else x : removeChar xs c

removeCharAt :: String -> Int -> Int -> String
removeCharAt [] _ _ = []
removeCharAt (x:xs) i s = if (i == s) then xs else x : removeCharAt xs i (s+1)

--substitutions

subsituteAt :: String -> Char -> Int -> Int -> String
subsituteAt [] _ _ _ = []
subsituteAt (x:xs) c i s = if (i == s) then c:xs else x : subsituteAt xs c i (s+1)

--transpositions

transposeAt :: String -> Int -> Int -> String
transposeAt [] _ _ = []
transposeAt [x] i s = [x] 
transposeAt (x:n:xs) i s = if (i == s) then n : x : xs else x : transposeAt (n:xs) i (s+1)

--feleisort

readAndSort :: String -> IO ()
readAndSort str = do
  str' <- readFile str
  print $ isort $ lines str'


--Game Trees

--Book Code Start

size :: Int
size = 3
  
data Tree a = Node a [Tree a]
  deriving Show

next :: Player -> Player
next O = X
next E = E
next X = O

full :: Grid -> Bool
full = all (/= E) . concat

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == E

won :: Grid -> Bool
won g = wins O g || wins X g

diag :: Grid -> [Player]
diag g = [g !! n !! n| n <- [0..size-1]]
  
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,E:ys) = splitAt i (concat g)
  
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]
  
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]
  
turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) (concat g))
    xs = length (filter (== X) (concat g))

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = L.transpose g
    dias = [diag g, diag (map reverse g)]

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
  | wins O g = Node (g,O) []
  | wins X g = Node (g,X) []
  | otherwise = Node (g,E) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps = [p | Node (_,p) _ <- ts']

depth :: Int
depth = 9

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree

type Grid = [[Player]]

data Player = O | E | X
  deriving (Eq, Ord, Show)

--Book Code End

--Minimax to strategy

fieldToPlayer :: Field -> Player
fieldToPlayer B = E
fieldToPlayer R = O
fieldToPlayer G = X

playerToField :: Player -> Field
playerToField E = B
playerToField O = R
playerToField X = G

gridToBoard :: Grid -> Board
gridToBoard g = [playerToField p | p <- (concat g)]

boardToGrid :: Board -> Grid
boardToGrid [] = []
boardToGrid board = [fieldToPlayer p | p <- (take 3 board)] : boardToGrid (drop 3 board)

findMove :: Board -> Board -> Int
findMove (n:org) (m:new) = if (n /= m) then 8 - length new else findMove org new

isEmpty :: Board -> Bool
isEmpty [] = True
isEmpty (f:b) = if (f == B) then isEmpty b else False

--Proving

findMoves :: Board -> [Int]
findMoves [] = []
findMoves (f:b) = if (f == B) then (8 - length b) : findMoves b else findMoves b

getPossibleBoards :: Board -> [Int] -> Field -> [Board]
getPossibleBoards _ [] _ = []
getPossibleBoards b ms p = [simMove b p m | m <- ms]

simMove :: Board -> Field -> Int -> Board
simMove [] _ _ = error "done goofed"
simMove (f:fs) p 0 = p : fs
simMove (f:fs) p i = f : simMove fs p (i-1)

isHorizontalWin :: Field -> Field -> Field -> Bool
isHorizontalWin a b c = if (a == b && b == c && filled) then True else False
  where filled = (a /= B) && (b /= B) && (c /= B)

isVerticalWin :: Field -> Field -> Field -> Bool
isVerticalWin a d g = if (a == d && d == g && filled) then True else False
  where filled = (a /= B) && (d /= B) && (g /= B)

isDiagonalWin :: Field -> Field -> Field -> Bool
isDiagonalWin a e i = if (a == e && e == i && filled) then True else False
  where filled = (a /= B) && (e /= B) && (i /= B)

isWon :: Board -> Bool
isWon [] = False
isWon (a:b:c:d:e:f:g:h:i:fs)
  | (isHorizontalWin a b c) || (isHorizontalWin d e f) || (isHorizontalWin g h i) = True
  | (isHorizontalWin a d g) || (isHorizontalWin b e h) || (isHorizontalWin c f i) = True
  | (isHorizontalWin a e i) || (isHorizontalWin c e g) = True
  | otherwise = False

isFull :: Board -> Bool
isFull [] = True
isFull (p:fs) = if (p == B) then False else isFull fs



freshBoard :: Board
freshBoard = [B,B,B,B,B,B,B,B,B]

--Red

--an adaptation of wins
isWonFancy :: Board -> Field -> Bool
isWonFancy b p = wins (fieldToPlayer p) (boardToGrid b)

--for every possible move Green can make, red must make a move that will not result it a loss for Red. The game is consideded to be not lost if the board is either won or full
--This functions creates a recursion tree from the possible moves that Green can make and recurses on the boards that are created from the moves that Red makes
proveForRed :: Board -> Bool
proveForRed mb = if ((isWonFancy mb R) == True || isFull mb) then True else if ((isWonFancy mb G) == True) then False else (and [proveForRed m | m <- redMoves])
  where bs = getPossibleBoards mb (findMoves mb) G
        redMoves = [simMove b R (strategyForRed b) | b <- bs]

--tests the proof for Red
--red must make the first move so Green can go, the resulting board is passed to proveForRed
proveRed :: Board -> Bool
proveRed b = proveForRed (simMove b R (strategyForRed b))

--Green

--for every possible move Red can make, red must make a move that will not result it a loss for Green. The game is consideded to be not lost if the board is either won or full
--This functions creates a recursion tree from the possible moves that Red can make and recurses on the boards that are created from the moves that Green makes
proveForGreen :: Board -> Bool
proveForGreen mb = if ((isWonFancy mb G) == True || isFull mb) then True else if ((isWonFancy mb R) == True) then False else (and [proveForGreen m | m <- bs])
  where greenMove = simMove mb G (strategyForGreen mb)
        bs = getPossibleBoards greenMove (findMoves greenMove) R

--tests the proof for Green
--it must find all the moves red can make and than pass them to the proveForGreen function
proveGreen :: Board -> Bool
proveGreen b = and [proveForGreen m | m <- (getPossibleBoards b (findMoves b) R)]
--proveGreen b = proveForGreen (simMove b R (strategyForRed b))

--This functions tests the proofs for Red and Green
showProofsForEmptyList :: String
showProofsForEmptyList = "Proving red: "++(show $ proveRed freshBoard)++"\n"++
  "Proving green: "++(show $ proveGreen freshBoard)++"\n"

--This functions returns the explanation text
getExplanationText :: String
getExplanationText = "This proof works by testing the minimax algorithm on every possible move that can be made by the opponent. When the board is either tied or won, the recursive tree collapses and if there are any losses, the proof functions return false, otherwise they return true. When they return true it means that there were no moves by the opponent that resulted in a loss.\n"

--CALL THE PROVE FUNCTION TO SEE THE PROOFS

--This functions displays the proofs
prove :: IO ()
prove = putStr $ getExplanationText ++ showProofsForEmptyList 



