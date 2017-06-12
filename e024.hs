import Data.List

main = do
    putStrLn $ (!! 999999) $ sort $ permutations "0123456789"