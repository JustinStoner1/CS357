\begin{code}
--5.1 Trees
data Tree a = E
            | T a (Tree a) (Tree a)
            deriving (Eq, Show)

example :: Tree Char
example = (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E))

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

https://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/icfp00bfn.pdf 

