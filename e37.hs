import Data.Numbers.Primes

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

fromDigits :: [Int] -> Int
fromDigits = foldl1 $ (+).(*10)

truncateRL :: Int -> [Int]
truncateRL n = trunc' init dig ++ trunc' tail dig
    where dig = digits n

trunc' :: ([Int] -> [Int]) -> [Int] -> [Int]
trunc' _ [] = []
trunc' fun ns = fromDigits ns:(trunc' fun (fun ns))

main = do
    print $ sum $ take 11 $ filter (all isPrime . truncateRL) $ drop 4 primes