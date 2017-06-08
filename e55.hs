isPalindrome :: Integer -> Bool
isPalindrome n = show n == (reverse $ show n)

revSum :: Integer -> Integer
revSum n = n + (read (reverse $ show n))

isLychrel :: Integer -> Bool
isLychrel n = lych' (revSum n) 49
lych' _ 0 = True
lych' n m = if (isPalindrome n) then False else lych' (revSum n) (m-1)

main = do
    print $ length $ filter isLychrel [5..10000]