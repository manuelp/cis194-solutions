{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Employee
import Data.Monoid
import Data.Tree

--
-- Exercise 1
--
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--
-- Exercise 2
--
--treeFold :: ? -> Tree a -> b
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

totalFun :: Tree Employee -> Fun
totalFun = treeFold (\e fs -> empFun e + sum fs) 
