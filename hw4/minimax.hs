import Data.Char
import Data.List
import System.IO

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
    cols = transpose g
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

--Book
type Grid = [[Player]]

data Player = O | E | X
  deriving (Eq, Ord, Show)
--Hw4
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field]

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
--boardToGrid (a:b:c:d:e:f:g:h:i:board) = [[fieldToPlayer a,fieldToPlayer b,fieldToPlayer c],[fieldToPlayer d,fieldToPlayer e,fieldToPlayer f],[fieldToPlayer g,fieldToPlayer h,fieldToPlayer i]]
boardToGrid [] = []
boardToGrid board = [fieldToPlayer p | p <- (take 3 board)] : boardToGrid (drop 3 board)