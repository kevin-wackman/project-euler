squares = map (^2) [1..]

isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse $ show n)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = drop from $ take to xs

allSlices = filter (<10^8) [sum $ slice x y squares | x<-[0..9998], y<-[(x+1)..9999]]

main = do
    print $ sum $ filter isPalindrome allSlices

