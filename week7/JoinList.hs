{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid
import Sized

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

-- instance Monoid m => Monoid (JoinList m a) where
--     mempty = Empty
--     mappend = (+++)

--
-- Exercise 2
--
indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single s v) = Just v
indexJ i (Append s a b)
       | Size i >= size s = Nothing
       | i < sublistSize a = indexJ i a
       | otherwise = indexJ (i - sublistSize a) b

sublistSize :: (Sized b, Monoid b) => JoinList b a -> Int
sublistSize = getSize . size . tag

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
