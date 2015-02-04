{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
                     deriving (Eq, Show)

--
-- Exercise 1
--
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

--
-- Exercise 2
--
indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single s v) = Just v
indexJ i (Append s a b)
       | Size i >= size s = Nothing
       | i < subListSize a = indexJ i a
       | otherwise = indexJ (i - subListSize a) b

subListSize :: (Sized b, Monoid b) => JoinList b a -> Int
subListSize = getSize . size . tag

-- For testing:

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

jl :: JoinList Size Char
jl = (Append (Size 4) (Append (Size 2) (Single (Size 1) 'a')
                                       (Single (Size 1) 'b'))
                      (Append (Size 2) (Single (Size 1) 'c')
                                       (Single (Size 1) 'd')))

testJl :: [(Int, Maybe Char)]
testJl = zip [0..5] (map (flip indexJ $ jl) [0..5])

-- Part 2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 l = l
dropJ n (Single _ _) = Empty
dropJ n (Append s l r)
      | subListSize l < n = dropJ (n - subListSize l) r
      | otherwise = (dropJ n l) +++ r

-- Check: 
-- jlToList (dropJ n jl) == drop n (jlToList jl)

-- Part 3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ n (Single _ _) = Empty
takeJ n (Append s l r)
      | subListSize l <= n = l +++ (takeJ (n - subListSize l) r)
      | otherwise = takeJ n l

-- Check:
-- jlToList (takeJ n jl) == take n (jlToList jl)

--
-- Exercise 3
--
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--
-- Exercise 4
--

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty
    mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString = mconcat . map enlist . lines
    line = indexJ
    replaceLine i s b = mconcat . map enlist $ (fst p ++ [s] ++ (tail . snd) p) 
                        where p = splitAt i (jlToList b)
    numLines = getSize . snd . tag
    value = getScore . fst . tag

enlist :: String -> JoinList (Score, Size) String
enlist s = Single (scoreString s, Size 1) s

buf :: JoinList (Score, Size) String
buf = (fromString . unlines)
      [ "This buffer is for notes you don't want to save, and for"
      , "evaluation of steam valve coefficients."
      , "To load a different file, type the character L followed"
      , "by the name of the file."
      ]

main :: IO ()
main = runEditor editor $ buf

