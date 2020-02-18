insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
    | x <= y = x : y : ys 
    | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

fib :: Int -> Int
fib 0 = 1;
fib 1 = 1;
fib n = fib (n - 2) + fib (n - 1)

-- Excersises
fact :: Int -> Int
fact 0 = 1
fact n | n > 0 = n * fact (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)

exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' a 1 = a
exp' a b | b > 0 = a * (exp' a (b - 1))

euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid (a - b) b
           | otherwise = euclid (b - a) a

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) 
    | x == False = False
    | otherwise = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [a] = a
concat' (y:xs) = y ++ concat' xs

nth' :: [a] -> Int -> a
nth' (x:xs) 1 = x
nth' (x:xs) n | n > 1 = nth' xs (n - 1) 

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) 
    | n == x = True
    | otherwise = elem' n xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] y = y
merge' x [] = x
merge' (x:xs) (y:ys)
    | x < y = x : merge' xs (y : ys)
    | otherwise = y : merge' (x : xs) ys