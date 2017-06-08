import Data.Numbers.Primes

main = do
    putStrLn $ show $ last $ take 10001 primes