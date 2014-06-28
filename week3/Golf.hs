module Golf where

---------- Exercise 1

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
genIndexes n m = map pred [n,n*2..m]

--------- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima l@[_,x2,_] = if maximum l == x2 then [x2] else []
localMaxima xs
  | length xs > 3 = localMaxima (take 3 xs) ++ localMaxima (tail xs)
  | otherwise = []

--------- Exercise 3
-- Builds a string that prints to an ASCII histogram showing, for every
-- integer from 0 to 9, how many times it appears in the input list.
histogram :: [Integer] -> String
histogram ns = unlines (reverse (histoRows ns) ++ ["==========","0123456789"])

-- Builds the rows of the histogram as a list of ordered strings. First
-- the row 1, then the row 2, etc.
histoRows :: [Integer] -> [String]
histoRows ns = (map (buildHistogramRow (calcFrequencies ns)) [1..9])

-- Here we define a type alias, just to be clearer.
type Frequencies = [Integer]

-- Since we are only interested in the occurences of integers from 0 to
-- 9, we just count them for these integers. For every number from 0 to
-- 9 (inclusive), we calculate its occurences in the input list and put
-- it into an ordered list. In other words, the output list built
-- calculating the cardinality of every number from 0 to 9 in the input
-- list.
calcFrequencies :: [Integer] -> Frequencies
calcFrequencies ns = map (countOccurences ns) [0..9]

-- We need a way to calculate how many times a given integer is present
-- in a list.
countOccurences :: [Integer] -> Integer -> Integer
countOccurences ns n = toInteger (length (filter (== n) ns))

-- Builds a string that represents an histogram row (for the integers
-- 0..9) for the given y coordinate, using the frequencies of every said
-- number.
buildHistogramRow :: Frequencies -> Integer -> String
buildHistogramRow frqs y = map (histoChar y) frqs

-- A particular number, in the given histogram row (y coordinate, or
-- level), should be represented with the '*' character if the number of
-- times it's present in the list is equal or greater than the level
-- we're at. Otherwhise, it should be represented by the ' ' character.
histoChar :: Integer -> Integer -> Char
histoChar level occurences
  | occurences >= level = '*'
  | otherwise = ' '
