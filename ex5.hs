replicate_ex n x = [ x | _ <- [1..n] ]

pyths :: Int -> [(Int, Int, Int)]
pyths sum = [ (x, y, z) | x <- [1..sum], y <- [1..sum], z <- [1..sum], x ^ 2 + y ^ 2 == z ^ 2]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [ x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (init (factors num)) == num

ex4 = concat [[ (x, y) | y <- [4, 5, 6], x <- [1, 2, 3] ]]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [ v | (k', v) <- t, k == k']

positions  :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [1..n])
    where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
-- scalarproduct xs ys = sum [ x * y | x <- xs, y <- ys]
scalarproduct xs ys = sum [ x * y | (x, y) <- xs `zip` ys]

ex8 = [(x,y) | x <- [1,2], y <- [1,2]]
ex9 = [x | x <- [1, 2, 3], _ <- [1..5]]
ex10 = sum[x | x <- [1..10], even x]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- zip xs ys]

divisors :: Int -> [Int]
divisors x = [n | n <- [1..x], divides x n]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0
