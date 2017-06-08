import Data.List
import Data.List.Split

main = do
    str <- readFile "e99in.txt"
    let pairs = map (splitOn ",") $ lines str
    let expValues = zip [1..] $ map (\[x,y] -> (read x)^(read y)) pairs
    print $ (\(x,y) -> x) $ head $ sortBy (\(x,x') (y,y') -> y' `compare` x') expValues