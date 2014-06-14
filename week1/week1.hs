------------ Exercise 1
takeDigit :: Integer -> Integer -> Integer
takeDigit num pos = (mod num (10^pos)) `div` (10^(pos-1))

------------ Exercise 2
takeAllButLastTwo xs = take (length xs - 2) xs
doubleAntepast xs = (xs !! (length xs - 2)) * 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther xs = (doubleEveryOther (takeAllButLastTwo xs)) ++ [doubleAntepast xs] ++ [last xs]

------------ Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits n = sum (map numSum n)

-- numSum :: Integer -> Integer
-- This signature is optional, Haskell's type-inference is powerful.
numSum n
  | (n <= 9)  = n
  | otherwise = takeDigit n 1 + takeDigit n 2

------------ Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (digitalize n)) `mod` 10) == 0

digitalize :: Integer -> [Integer]
digitalize n = map (takeDigit n) [(numDigits n),(numDigits n)-1..1]
-- Thanks to currying, we don't need to use `partial` or lambdas like in Clojure for the
-- first `map`'s argument.

numDigits :: Integer -> Integer
numDigits n = fromIntegral (length (show n))

------------ Exercise 5 (Tower of Hanoi)
-- Moves an Hanoi tower from the first peg to the second one.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
