module HW01
(
 toDigits,
 toDigitsRev,
 doubleEveryOther,
 sumDigits,
 validate,
 hanoi
) where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x < 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x) = (reverse . doubleEveryOther' . reverse) (x)
doubleEveryOther' [] = []
doubleEveryOther' (x:[]) = [x]
doubleEveryOther' (x:(y:zs)) = x : (2*y) : doubleEveryOther' zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = (sum . toDigits) x  + sumDigits xs

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a 
