-- Неправильное вычетание

main = interact $ last . (\[n,k] -> take (read k + 1) $ iterate g n) . words 
  where g s | head b == '0'  = a
            | otherwise = a ++ (show $ read b - 1)
          where (a,b) = splitAt (length s - 1) s    
