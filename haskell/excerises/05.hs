sumSquares :: Int -> Int
sumSquares n = sum [x^2 | x <- [0..n]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(a, b) | a <- [0..m], b <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(a, b) | (a, b) <- (grid n n), a /= b]

replicate' :: Int -> a -> [a]
replicate' n t = [t | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [0..n], y <- [0..n], z <- [0..n], x > 0, y > 0, z > 0, x * x + y * y == z * z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n - 1], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x)) == x]

scalerP :: [Int] -> [Int] -> Int
scalerP as bs = sum [a * b | (a, b) <- (zip as bs)] 
