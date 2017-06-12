import Data.List

testNum :: Int -> String
testNum n = (show n) ++ (show (n*2))

isPandigital9 :: String -> Bool
isPandigital9 = (== ['1'..'9']) . sort

main = do
    putStrLn $ maximum $ filter isPandigital9 $ map testNum [9123..9876]