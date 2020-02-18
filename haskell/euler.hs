import Text.Regex.Base

product' :: (Num a) => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

factorial n = product [1..n]

average :: Foldable t => t Int -> Int
average ns = (sum ns) `div` (length ns)

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last xs

init' :: [a] -> [a]
init' [a] = []
init' (x:xs) = [x] ++ init xs

html = "<div>hello</div>"

length' [] = 0
length' (x:xs) = 1 + length xs

finnishTagName :: [Char] -> [Char]
finnishTagName (x:xs) | x == '>' = ['>']
                  | otherwise = [x] ++ getTagName xs


