import Data.List.Split
import Data.List
import Data.Char

parse :: String -> [String]
parse = splitOn ","

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

getWordValue :: String -> Int
getWordValue = foldl (\n x -> n+(ord x)-64) 0 


main = do
    str <- readFile "e022in.txt"
    print $ sum $ zipWith (*) [1..] $ map getWordValue $ sort $ parse $ stripQuotes str