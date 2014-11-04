(!!!) :: Int -> [Int] -> Int
0 !!! (x: _) = x
_ !!! [] = 0
n !!! (_:xs) = (n-1) !!! xs

elemex :: Eq a => a -> [a] -> Bool
elemex _ [] = False
elemex x (y : ys)
    | x == y = True
    | otherwise = elemex x ys

mergex :: Ord a => [a] -> [a] -> [a]
meregex [] [] = []
mergex [] x = x
mergex x [] = x
mergex (x:xs) (y:ys)
    | x > y =  y : x : mergex xs ys
    | y >= x  =  x : y : mergex xs ys
