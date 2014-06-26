module Golf where

-- Exercise 1

-- Builds a list with the series of nth and multiples elements in the
-- input list for every valid index position in it.
skips :: [a] -> [[a]]
skips [] = []
skips l = map (takeNths l) [1..(length l)]

-- Takes all the nth elements of the list, starting with a given 1-based
-- index and all the multiples up until the end of the input list.
takeNths :: [a] -> Int -> [a]
takeNths l 1 = l
takeNths l first = map (l !!) (genIndexes first (length l))

-- Generates a list with the predecessors of all the multiples of the
-- given one until the maximum.
genIndexes :: Int -> Int -> [Int]
genIndexes _ 0 = []
genIndexes n s = map pred [n,n*2..s]
