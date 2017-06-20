digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

fifthPower :: Int -> Bool
fifthPower x = x == (sum $ map (\x -> x^5) $ digits x)

main = do
    print $ sum $ filter fifthPower [2..(9^5)*6]