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

numSum n
  | (n <= 9)  = n
  | otherwise = takeDigit n 1 + takeDigit n 2

