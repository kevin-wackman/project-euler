readNumeral :: String -> Int
readNumeral [] = 0
readNumeral ('M':xs)        = readNumeral xs + 1000
readNumeral ('C':'M':xs)    = readNumeral xs + 900
readNumeral ('D':xs)        = readNumeral xs + 500
readNumeral ('C':'D':xs)    = readNumeral xs + 400
readNumeral ('C':xs)        = readNumeral xs + 100
readNumeral ('X':'C':xs)    = readNumeral xs + 90
readNumeral ('L':xs)        = readNumeral xs + 50
readNumeral ('X':'L':xs)    = readNumeral xs + 40
readNumeral ('X':xs)        = readNumeral xs + 10
readNumeral ('I':'X':xs)    = readNumeral xs + 9
readNumeral ('V':xs)        = readNumeral xs + 5
readNumeral ('I':'V':xs)    = readNumeral xs + 4
readNumeral ('I':xs)        = readNumeral xs + 1

writeNumeral :: Int -> String
writeNumeral n
    | n <=0     = []
    | n >= 1000 = 'M':(writeNumeral     (n-1000))
    | n >= 900  = 'C':'M':(writeNumeral (n-900))
    | n >= 500  = 'D':(writeNumeral     (n-500))
    | n >= 400  = 'C':'D':(writeNumeral (n-400))
    | n >= 100  = 'C':(writeNumeral     (n-100))
    | n >= 90   = 'X':'C':(writeNumeral (n-90))
    | n >= 50   = 'L':(writeNumeral     (n-50))
    | n >= 40   = 'X':'L':(writeNumeral (n-40))
    | n >= 10   = 'X':(writeNumeral     (n-10))
    | n >= 9    = 'I':'X':(writeNumeral (n-9))
    | n >= 5    = 'V':(writeNumeral     (n-5))
    | n >= 4    = 'I':'V':(writeNumeral (n-4))
    | n >= 1    = 'I':(writeNumeral     (n-1))

main = do
    text <- readFile "e089in.txt"
    let numerals = lines text
    let preLength = sum $ map length numerals
    let newmerals = map writeNumeral $ map readNumeral numerals
    let postLength = sum $ map length newmerals
    print $ preLength - postLength