import Data.List
import Data.Ord
import Math.NumberTheory.Powers.Squares

triangles :: Int -> [(Int, Int, Int)]
triangles n = let
    half = n `div` 2
    in [(a,b,integerSquareRoot(a^2+b^2)) | a<-[3..half], b<-[a..half], isSquare (a^2+b^2), a+b+(integerSquareRoot' (a^2+b^2)) == n]
    
main = do
    print $ fst $ last $ sortBy (comparing snd) $ filter (\(_,y) -> y>3) $ zipWith (\x y -> (x,(length $ triangles y))) [10..] [10..1000]