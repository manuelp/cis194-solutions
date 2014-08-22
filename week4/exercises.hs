{-# OPTIONS_GHC -Wall #-}

-- ---------- Exercise 1
-- Exercise 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

-- Exercise 1.2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x 
                                                            then (x `div` 2) 
                                                            else (3 * x + 1))

-- ---------- Exercise 2
data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf 

addNode :: a -> Tree a -> Tree a
addNode e Leaf = Node 0 Leaf e Leaf
addNode e (Node 0 Leaf v Leaf) = Node 1 (addNode e Leaf) v Leaf
addNode e (Node h l v Leaf) = Node h l v (addNode e Leaf)
addNode e (Node h Leaf v r) = Node h (addNode e Leaf) v r
addNode e (Node h l@(Node hl _ _ _) v r@(Node hr _ _ _))
    | hl <= hr = case (addNode e l) of 
                   (Node hl' _ _ _) -> Node (if hl' > hr then h + 1 else h) (addNode e l) v r
    | otherwise = case (addNode e r) of
                    (Node hr' _ _ _) -> Node (if hr' > hl then h + 1 else h) l v (addNode e r)

-- foldTree "ABCDEFGHIJ" ==
--          Node 3
--                   (Node 2
--                             (Node 0 Leaf ’F’ Leaf)
--                             ’I’
--                             (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--                   ’J’
--                   (Node 2
--                             (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--                             ’H’
--                             (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
                  

-- ---------- Exercise 3.1
xor :: [Bool] -> Bool
xor = odd . foldl (\c x -> if x then c+1 else c) 0  

-- ---------- Exercise 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> [(f a)] ++ bs) [] 

-- ---------- Exercise 3.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs) 

-- ---------- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x+1) . (\xs -> filter (\x -> null (calcSolutions x (last xs))) xs) . (\x -> [1..x])

calcSolutions :: Integer -> Integer -> [(Integer, Integer)]
calcSolutions x n = [(i,j) | (i,j) <- cartProd [1..n] [1..n], i<=j, i+j+2*i*j<=n, i+j+2*i*j==x]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
