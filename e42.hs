import Data.List.Split
import Data.Char

tris = scanl (+) 1 [2..]

isTriangle :: Int -> Bool
isTriangle n = n == (head $ dropWhile (< n) tris)

parse :: String -> [String]
parse = splitOn ","

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

getWordValue :: String -> Int
getWordValue xs = foldl (\n x -> n+(ord x)-64) 0 xs

main = do
    str <- readFile "e42in.txt"
    let strings = parse $ stripQuotes str
    print $ length $ filter isTriangle $ map getWordValue strings