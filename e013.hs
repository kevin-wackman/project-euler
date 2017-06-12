parse :: String -> [Integer]
parse = (map read) . lines

main = do
    str <- readFile "e13in.txt"
    let arr = parse str
    let sumStr = show $ sum arr
    putStrLn $ take 10 sumStr