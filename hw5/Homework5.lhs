Justin Stoner
Jstoner
\begin{code}
import Data.List

--5.1 Trees
data Tree a = E
            | T a (Tree a) (Tree a)
            deriving (Eq, Show)

--example :: Tree Char
--example = (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E))

\end{code}
gets the two children from the binary tree received as input
\begin{code}
getChildren :: Tree a -> [Tree a]
getChildren E = []--No children to grab
getChildren (T _ t1 t2) = [t1, t2]--grab the children from the input branch and put them into a list

\end{code}
puts the tree back together using a list of the layers of the orginal tree as a guide but with the numeric labels instead of whatever was there before
\begin{code}
rebuildTree :: Int -> [Tree a] -> [Tree Int] -> [Tree Int]
rebuildTree x [] [] = []--stop if the tree is rebuilt
rebuildTree x (E : ts) cs = E : rebuildTree x ts cs--stop building that root and move onto the next branch
rebuildTree x ts (a : b : cs) = T x a b : rebuildTree (x+1) (tail ts) cs--rebuild the tree using a skeleton version of the input tree with the new labels produced in bfnumProxy

\end{code}
creates a list of children in breadth first order and sends it to rebuild along with the starting index for that level
\begin{code}
bfnumProxy :: Int -> [Tree a] -> [Tree Int]
bfnumProxy _ [] = [] --No tree to label
bfnumProxy i lvl = rebuildTree i lvl nextLvl'
  where nextLvl = concatMap getChildren lvl--a list of the children of the current nodes on the current level
        j = i + (div (length nextLvl) 2)--starting index made from the last index used and the length of the new level
        nextLvl' = bfnumProxy j nextLvl--makes the next breadth-first level and sends it to be processed the same way as the last one

\end{code}
calls bfnumProxy with the starting values
\begin{code}
bfnum :: Tree a -> Tree Int
bfnum t = head (bfnumProxy 1 [t])--reterive the product from bfnumProxy

\end{code}

I have adapted and edited the code found in section 4 "Level-oriented solutions" of the paper by Chris Okasaki titled "Breath-First Numbering". I choose this particular solution because I understood it better than the others, hence why I have re-written it in haskell. 

URL for the paper:
https://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/icfp00bfn.pdf 
\begin{code}

--5.2 Expression Trees
type Identifier = String

data Expr = Num Integer
          | Var Identifier
          | Let {var :: Identifier, value :: Expr, body :: Expr}
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq)

instance Show Expr where
  show (Num x) = show x
  show (Var c) = c
  show (Let c v e) = "Let "++c++" = "++(show v)++" in "++(show e)++" end"
  show (Add e1 e2) = (show e1)++" + "++(show e2)
  show (Sub e1 e2) = (show e1)++" - "++(show e2)
  show (Mul e1 e2) = (show e1)++" * "++(show e2)
  show (Div e1 e2) = (show e1)++" / "++(show e2)

type Env = Identifier -> Integer

emptyEnv :: Env
emptyEnv = \s -> error ("unbound: " ++ s)

extendEnv :: Env -> Identifier -> Integer -> Env
extendEnv oldEnv s n s' = if s' == s then n else oldEnv s'

evalInEnv :: Env -> Expr -> Integer
evalInEnv env exp = evalProxy env exp

eval :: Expr -> Integer
eval e = evalInEnv emptyEnv e

--5.3 Infinite Lists
diag :: [[a]] -> [a]
diag xs = concat [getDiagonals xs n | n <- [0..]]


------------------------------------------------
-----------Helper-----------Functions-----------
------------------------------------------------


--bfnum


example :: Tree Char
example = (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E))

breadthFirstListify :: Tree a -> [a]
breadthFirstListify E = []
breadthFirstListify (T x t1 t2) = undefined

compressTuple :: (a, a) -> [a]
compressTuple (a, b) = a:[b]

getBranches :: Tree a -> (Tree a, Tree a)
getBranches (T x t1 t2) = (t1,t2)

--evalInEnv

evalProxy :: Env -> Expr -> Integer
evalProxy _ (Num x) = x
evalProxy env (Var c) = env c
evalProxy env (Let c v e) = evalProxy (\x -> (evalProxy env v)) e
evalProxy env (Add e1 e2) = (evalProxy env e1) + (evalProxy env e2)
evalProxy env (Sub e1 e2) = (evalProxy env e1) - (evalProxy env e2)
evalProxy env (Mul e1 e2) = (evalProxy env e1) * (evalProxy env e2)
evalProxy env (Div e1 e2) = div (evalProxy env e1) (evalProxy env e2)

--diag

getDiagonals :: [[a]] -> Int -> [a]
getDiagonals xs n = [get n2 $ get n1 xs | (n1, n2) <- getDiagCords 0 n]

getDiagCords :: Int -> Int -> [(Int,Int)]
--getDiagCords 0 m = [(fromIntegral 0,m)]
getDiagCords n 0 = [(n,fromIntegral 0)]
getDiagCords n m = (n,m) : getDiagCords (n+1) (m-1)

getDiagonalEnds :: [[a]] -> Int -> [a]
getDiagonalEnds xs n = [get n $ get 0 xs] ++ [get 0 $ get n xs]

get :: Int -> [a] -> a
get i xs = head $ drop i xs

--Test Code

rlist = [ [i/j | i <- [1..]] | j <- [1..] ]
qlist1 = [[show i ++ "/" ++ show j | i <- [1..]] | j <- [1..]]
qlist2 = [[fracString i j | i <- [1..]] | j <- [1..]]

fracString :: Integer -> Integer -> String
fracString num den = if denominator == 1
  then show numerator
  else show numerator ++ "/" ++ show denominator
    where c = gcd num den
          numerator = num `div` c
          denominator = den `div` c

block :: Int -> [[a]] -> [[a]]
block n x = map (take n) (take n x)


--


\end{code}

