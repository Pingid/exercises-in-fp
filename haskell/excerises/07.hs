import Data.Char (ord, chr)

type Bit = Int

unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + y * 2) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 bits
    | length bits > 8 = take 8 bits : chop8 (drop 8 bits)
    | otherwise = [bits]

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold ((== 0) . length) (take 8) (drop 8)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = False
all' fn (x:[]) = fn x
all' fn (x:xs) = fn x && (all' fn xs)

all'' :: (a -> Bool) -> [a] -> Bool
all'' fn = foldr (\x y -> y && (fn x)) True

any' :: (a -> Bool) -> [a] -> Bool
any' fn = foldr (\x y -> y || (fn x)) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' fn (x:xs)
    | fn x = x : takeWhile' fn xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' fn (x:xs)
    | fn x = dropWhile' fn xs
    | otherwise = x : xs

curry' :: ((a, b) -> c) -> a -> b -> c
curry' fn a b = fn (a, b) 

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x + y * 2) 0