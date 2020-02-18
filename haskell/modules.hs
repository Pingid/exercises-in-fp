import Data.List (nub, sort, intersperse, intercalate, transpose)
import qualified Data.Map as M
-- import qualified Data.Maybe

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

intersperse' :: a -> [a] -> [a]
intersperse' _ (x:[]) = x : []
intersperse' a (x:xs) = x : a : (intersperse a xs)

intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs (y:[]) = y
intercalate' xs (y:ys) = y ++ xs ++ (intercalate' xs ys)

-- transpose' :: [[a]] -> [[a]]
