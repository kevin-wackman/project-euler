isPalindrome :: Integer -> Bool
isPalindrome n = show n == (reverse $ show n)

toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]
    
fromDigits :: [Integer] -> Integer
fromDigits = foldl1 $ (+).(*10)

toBinary :: Integer -> Integer
toBinary = fromDigits . toBin

isBinPalindrome :: Integer -> Bool
isBinPalindrome = isPalindrome . toBinary

main = do
    print $ sum $ filter isBinPalindrome $ filter isPalindrome [1..999999]