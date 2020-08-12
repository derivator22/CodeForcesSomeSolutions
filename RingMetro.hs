module Main where

-- Кольцевое метро 

main = interact $ (\[n,a,x,b,y] -> g n x y (a,b)) . map read . words
 
g :: Int -> Int -> Int -> (Int,Int) -> String
g n x y (a,b)
  | (a == mod x n || b == mod y n) = if a /= b then "NO" else "YES"
  | a == b = "YES"
  | otherwise = g n x y (mod (a+1) n, mod (b-1) n)
