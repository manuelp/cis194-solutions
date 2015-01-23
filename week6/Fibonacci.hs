{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

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
