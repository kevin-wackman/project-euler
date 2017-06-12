import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

squareAndSum :: Int -> Int
squareAndSum = sum . map (^2) . digits

testChain :: Int -> Bool
testChain 1 = False
testChain 89 = True
testChain n = testChain $ squareAndSum n

main = do
    print $ length $ filter testChain [1..10000000]