-- Div на Mod

module Main where  
 
import qualified Data.Text.IO as T
import qualified Data.Text as T
 
main = do
  t <- T.getLine
  putStrLn
    $ show
    $ (\[n,k] -> sol n k 1 (n*k + k))
    $ map read
    $ words
    $ T.unpack t
 
sol :: Int -> Int -> Int -> Int -> Int
sol n k i res | n == 1       = k+1
              | i > k-1      = res 
              | mod n i == 0 = if res' < res
                               then sol n k (i+1) res' 
                               else sol n k (i+1) res 
              | True         = sol n k (i+1) res
  where res' = k * (div n i) + i
