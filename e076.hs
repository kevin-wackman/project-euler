next :: (Integral a) => a -> a
next = (+(-1))
--                                  type  value
findCombinations :: (Integral a) =>  a  ->  a  -> a
findCombinations 1 _ = 1
findCombinations 2 n = n `div` 2 + 1
findCombinations t n
    | n < t     = findCombinations (next t) n
    | otherwise = findCombinations (next t) n + (findCombinations t (n-t))
    
main = do
    print $ (+(-1)) $ findCombinations 100 100