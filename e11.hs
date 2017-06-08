getVal :: [[a]] -> Int -> Int -> a
getVal ns row col = (ns !! row) !! col

getRow4 :: [[a]] -> Int -> Int -> [a]
getRow4 ns row y = map (getVal ns row) [y..y+3]

getCol4 :: [[a]] -> Int -> Int -> [a]
getCol4 ns x col = map (\row -> getVal ns row col) [x..x+3]

getDDiag4 :: [[a]] -> Int -> Int -> [a]
getDDiag4 ns x y = map (\(row,col) -> getVal ns row col) $ zip [x..x+3] [y..y+3]

getUDiag4 :: [[a]] -> Int -> Int -> [a]
getUDiag4 ns x y = map (\(row,col) -> getVal ns row col) $ zip [x..x+3] (reverse [y-3..y])

getAllRows :: (Num a) => [[a]] -> [a]
getAllRows xs = map product [getRow4 xs row col | row<-[0..19], col<-[0..16]]

getAllCols :: (Num a) => [[a]] -> [a]
getAllCols xs = map product [getCol4 xs row col | row<-[0..16], col<-[0..19]]

getAllDDiags :: (Num a) => [[a]] -> [a]
getAllDDiags xs = map product [getDDiag4 xs row col | row<-[0..16], col<-[0..16]]

getAllUDiags :: (Num a) => [[a]] -> [a]
getAllUDiags xs = map product [getUDiag4 xs row col | row<-[0..16], col<-[3..19]]

main = do
    str <- readFile "e11in.txt"
    let nums = map (map read) $ map words $ lines str :: [[Int]]
    print $ map (\fun -> maximum (fun nums)) [getAllRows, getAllCols, getAllDDiags, getAllUDiags]