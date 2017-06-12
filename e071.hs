import Data.Ratio
import Data.List

main = do
    print $ (+ (-1)) $ (*3) $ (`div` 7) $ last $ takeWhile (<1000000) $ map (*7) [1..]
