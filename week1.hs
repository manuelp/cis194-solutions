-- Exercise 1
takeDigit :: Integer -> Integer -> Integer
takeDigit num pos = (mod num (10^pos)) `div` (10^(pos-1))

-- Exercise 2
takeAllButLastTwo xs = take (length xs - 2) xs
doubleAntepast xs = (xs !! (length xs - 2)) * 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther xs = (doubleEveryOther (takeAllButLastTwo xs)) ++ [doubleAntepast xs] ++ [last xs]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits n = sum (map numSum n)

-- numSum :: Integer -> Integer
-- La segnatura non è necessaria grazie alla superba type-inference di Haskell :)
numSum n
  | (n <= 9)  = n
  | otherwise = takeDigit n 1 + takeDigit n 2

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (digitalize n)) `mod` 10) == 0

digitalize :: Integer -> [Integer]
digitalize n = map (takeDigit n) [(numDigits n),(numDigits n)-1..1]
-- Grazie al currying, non serve usare partial o usare una lambda per la funzione da dare
-- in pasto a map :)

numDigits :: Integer -> Integer
numDigits n = fromIntegral (length (show n))

-- Exercise 5 (Tower of Hanoi)
-- Sposta la torre di Hanoi dal primo Peg al secondo, usando il terzo come temporaneo
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

-- Bellissimo, molto conciso e naturale. Non è tail-recursive, ma compilato e tutto
-- funziona tranquillamente senza far esplodere lo stack o l'heap. Cfr con l'implementazione
-- in Clojure che è meno bella (certo, il motivo è che la JVM non fornisce la TCO).
