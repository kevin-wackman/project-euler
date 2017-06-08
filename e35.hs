import Data.Numbers.Primes

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = take (length xs) (drop n (cycle xs))

listPerms :: [a] -> [[a]]
listPerms xs = map (\n -> rotate n xs) [0..((length xs)-1)]

getPerms :: Int -> [Int]
getPerms = map read . listPerms . show

isCircular :: Int -> Bool
isCircular = all isPrime . getPerms

main = do
    print $ length $ filter isCircular $ takeWhile (< 1000000) primes