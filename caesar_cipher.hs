import Data.Char

let2int :: Char -> Int
let2int c 
    | isLower c = ord c - ord 'a'
    | isUpper c = ord c - ord 'A'
    | otherwise = 0

int2let :: Int -> Char 
int2let n = chr (ord 'a' + n)

cint2let :: Int -> Char 
cint2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let((let2int c + n) `mod` 26)
    | isUpper c = cint2let((let2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


