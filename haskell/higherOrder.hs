zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

reduce' :: (a -> b -> a) -> a -> [b] -> a
reduce' f n [] = n
reduce' f n (x:xs) = reduce' f (f n x) xs

oddSquarBellow :: Int -> Int
oddSquarBellow x = sum (takeWhile (<x) (filter odd (map (^2) [1..])))

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x
  | odd x  = x : collatz (3 * x + 1)
  | even x = x : collatz (x `div` 2)

-- lengthGreaterThanBellow :: Int -> Int -> Int
-- lengthGreaterThanBellow x y = length (map length (filter (\a -> length a > x && ) (map collatz [y,y-1..])))

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldl (\acc y -> if y == a then True else acc) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: (Ord a) => [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldl (\acc x -> if f x then x : acc else acc) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- COMPOSE
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
pointed x = ceiling (negate (tan (cos (max 50 x))))
pointFree = ceiling . negate . tan . cos. max 50

dollerCompose = map (+1) $ map (+2) $ map (+3) [1..10]
dollerFuncCompose = map ($ 3) [(3+), (3-), (3*), sqrt]

testCompose = take(10) $ map (negate . sum . tail) (repeat [1..10])

sumOddSquaresBellow x = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..x]
