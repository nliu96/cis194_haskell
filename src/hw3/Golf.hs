module Golf
(
  skips,
  localMaxima,
  histogram
) where

skips :: [a] -> [[a]]
skips xs = zipWith every [1..length xs] (repeat xs)

every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram x = concat $ generateString $ generateList x

generateString :: [Integer] -> [String]
generateString x = [generateRowString x (maximum x) y | y <- [1..(maximum x)]] ++ (replicate 10 "=") ++ ["\n0123456789"]

generateRowString :: [Integer] -> Integer -> Integer -> String
generateRowString (x:xs) max y
  | max - x < y = "*" ++ (generateRowString xs max y)
  | otherwise = " " ++ generateRowString xs max y
generateRowString _ _ _ = "\n"

generateList :: [Integer] -> [Integer]
generateList x = [count x y | y <- [0..9]]

count :: [Integer] -> Integer -> Integer
count [] _ = 0
count (x:xs) y
  | x == y = 1 + count xs y
  | otherwise = count xs y
