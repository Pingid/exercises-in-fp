data List a = Nil | Cons a (List a)


make :: [a] -> List a
make [] = Nil
make (x:xs) = Cons x (make xs)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
    | x == y = True
    | x < y = occurs x l
    | otherwise = occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- Classes instances
class Eqq a where
    (===), (/==) :: a -> a -> Bool
    x /== y = not (x === y)

instance Eqq Bool where
    False === False = True
    True  === True  = True
    _     === _     = False

class Eqq a => Ordd a where
    (<<), (<==), (>>), (>==) :: a -> a -> Bool
    min, max             :: a -> a -> a

    min x y | x <== y = x
            | otherwise = y
            
    max x y | x <== y = y
            | otherwise = x

instance Ordd Bool where
    False << True = True
    _     << _    = False

    b <== c = (b << c) || (b == c)
    b >> c = c << b
    b >== c = c <== b

class Printable a where
    stringify :: a -> String

instance Printable List where
    stringify Nil = ""
    stringify (Cons a xs) = show a : stringify xs