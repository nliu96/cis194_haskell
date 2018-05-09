module Golf
(
  skips,
  localMaxima
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
