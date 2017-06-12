import Data.Char

digits :: Int -> [Int]
digits = (map digitToInt) . show

termini = [1,2,145,40585,871,45361,872,45362,169,363601,1454]
termini1 = [1,2,145,40585]
termini2 = [871,45361,872,45362]
termini3 = [169,363601,1454]

facNums = 1:(scanl1 (*) [1..9])

fac :: Int -> Int
fac = (facNums !!)

facDigits :: Int -> Int
facDigits = sum . map fac . digits

addAppropriate :: Int -> Int
addAppropriate n
    | elem n termini1 = 1
    | elem n termini2 = 2
    | elem n termini3 = 3

is60Chain :: Int -> Bool
is60Chain n = countChain n 0

countChain :: Int -> Int -> Bool
countChain num times
    | elem num termini = (addAppropriate num + times) == 60
    | times > 60 = False
    | otherwise = countChain (facDigits num) (times+1)
    
main = do
    print $ length $ filter is60Chain [1..999999]