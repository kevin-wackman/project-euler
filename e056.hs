digits :: Integer -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [fromInteger $ mod x 10]

nums = [a^b | a<-[1..100], b<-[1..100]]
main = do
    print $ maximum $ map sum $ map digits nums