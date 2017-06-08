champ = concat $ map show [1..]

digits :: Integer -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [fromInteger $ mod x 10]

main = do
    print $ product $ digits $ read $ map (\n -> (champ!!(n-1))) [1,10,100,1000,10000,100000,1000000]