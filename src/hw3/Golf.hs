module Golf
(
  skips
) where

skips :: [a] -> [[a]]
skips xs = zipWith every [1..length xs] (repeat xs)

every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []
