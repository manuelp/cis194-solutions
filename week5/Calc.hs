{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where
import ExprT
import Parser
import qualified StackVM as SVM

-- ---------- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- ---------- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = maybe Nothing (\e -> Just (eval e)) . (parseExp Lit Add Mul)

-- ---------- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id
-- reify $ add (lit 1) (lit 2)

-- ---------- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (&&)
    mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a+b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a*b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- ---------- Exercise 5

-- Instance of Expr type class that implements the calculation
-- functions in such a way that Parser can use them to compute the
-- result: in this case a Program.
instance Expr SVM.Program where
    lit n = [(SVM.PushI n)]
    add p1 p2 = p1 ++ p2 ++ [SVM.Add]
    mul p1 p2 = p1 ++ p2 ++ [SVM.Mul]

-- A compiler takes a string representation of the calculation (as
-- we've seen before), and compiles it to a Program.  We use parseExp
-- to take advantage of the Expr type class (specialized for Program)
-- to build the compiled stack of expressions ready for the VM.
compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

exec :: String -> Either String SVM.StackVal
exec s = case (compile s) of
           (Just p) -> SVM.stackVM p
           Nothing -> Left "Invalid stuff!"
      
--
-- Exercise 6
--
class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = M.lookup s
  
-- varLooker :: String -> M.Map String Integer -> Maybe Integer
-- varLooker s = var s
-- varLooker "Hello" $ M.singleton "Hello" 5

-- class Expr a where
--     lit :: Integer -> a
--     add :: a -> a -> a
--     mul :: a -> a -> a

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = (\m -> Just n)
    add a b = (\m -> if DM.isJust (a m) && DM.isJust (b m) 
                     then Just $ foldl (+) 0 (DM.catMaybes [(a m), (b m)])
                     else Nothing)
    mul a b = (\m -> if DM.isJust (a m) && DM.isJust (b m) 
                     then Just $ foldl (*) 1 (DM.catMaybes [(a m), (b m)])
                     else Nothing)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
