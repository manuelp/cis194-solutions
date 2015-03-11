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

--
-- Exercise 3
--
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (maximum (map (glCons e . snd) gls),
                   maximum (map fst gls))

-- Some "tests"
test1 :: (GuestList, GuestList)
test1 = nextLevel (Emp "A" 1) [(GL [] 0, GL [] 0)]

test2 :: (GuestList, GuestList)
test2 = nextLevel (Emp "A" 1) [(GL [(Emp "bossB" 3), (Emp "B" 2)] 5, 
                                GL [(Emp "B" 2)] 2)]
