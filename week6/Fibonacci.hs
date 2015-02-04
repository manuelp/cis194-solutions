{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

import Data.List

--
-- Exercise 1
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--
-- Exercise 2
--
fibs2 :: [Integer]
fibs2 = foo [0,1]

foo :: [Integer] -> [Integer]
foo [x1,x2] = foo [x1,x2,x1+x2]
foo xs
    | length xs > 2 = take (length xs - 2) xs 
                      ++ 
                      foo (reverse . take 2 . reverse $ xs)
    | otherwise = xs

--
-- Exercise 3
--
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

--
-- Exercise 4
--
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

--
-- Exercise 5
--
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) 
                          (streamMap largestPow2 (streamFromSeed (+2) 2))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a (Cons b (interleaveStreams as bs))

largestPow2 :: Integer -> Integer
largestPow2 = toInteger . length . pows2

pows2 :: Integer -> [Integer]
pows2 = unfoldr (\b -> if b `mod` 2 == 0
                       then Just (b, b `div` 2)
                       else Nothing) 

