import Data.Numbers.Primes

primes' =reverse $ take 20 primes

next :: (Integral a) => a -> a
next = (+(-1))
--                                  type  value
findCombinations :: (Integral a) =>  Int  ->  a  -> a
findCombinations 0 n 
    | odd n     = 0
    | otherwise = 1
findCombinations t n
    | n < (primes !! t) = findCombinations (next t) n
    | otherwise         = findCombinations (next t) n + (findCombinations t (n-(primes !! t)))
    
main = do
    print $ head $ filter (\x -> (findCombinations 200 x) > 5000) $ [2..]