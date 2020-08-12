-- Расшифровка

main = interact $ (\[n,s] -> decode (read n) s "") . words
 
decode :: Int -> String -> String -> String
decode _ "" res = res
decode n (x:xs) res | even n    = decode (n - 1) xs (x : res) 
                    | otherwise = decode (n - 1) xs (res ++ [x])
