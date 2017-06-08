import Data.List

isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse $ show n)

prods :: [Int]
prods = [x*y | x<-(reverse [100..999]), y<-(reverse [x..999])]

main = do
    print $ head $ filter isPalindrome $ sortBy (flip compare) prods