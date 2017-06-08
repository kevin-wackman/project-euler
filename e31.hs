next :: (Integral a) => a -> a
next 2   = 1
next 5   = 2
next 10  = 5
next 20  = 10
next 50  = 20
next 100 = 50
next 200 = 100

--                                  type  value
findCombinations :: (Integral a) =>  a  ->  a  -> a
findCombinations 1 _ = 1
findCombinations 2 n = n `div` 2 + 1
findCombinations t n
    | n < t     = findCombinations (next t) n
    | otherwise = findCombinations (next t) n + (findCombinations t (n-t))
    
main = print $ findCombinations 200 200