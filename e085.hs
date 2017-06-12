import Data.List
type Area = (Int, Int)


numSpecificRectangles :: Area -> Area -> Int
numSpecificRectangles (totalX,totalY) (specX,specY) = (totalX-specX+1) * (totalY-specY+1)

numRectangles :: Area -> Int
numRectangles (x,y) = sum $ map (numSpecificRectangles (x,y)) [(x',y') | x'<-[1..x], y'<-[1..y]]

main = do
    print $ head $ sortBy (\(x,y) (x',y') -> y `compare` y') $ map (\x -> (x,(abs (2000000-(numRectangles x))))) [(x,y) | x<-[53..80], y<-[1..x]]