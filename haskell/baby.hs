doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else 2 * x
lostNumbers = [1,2,3,4,5]

-- Return this | take item from list | only if
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

spam :: Int -> String
spam i = concat (replicate i "hue")
triangles x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x]]
rightTriangles x = [(a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1;
factorial x = x * factorial (x - 1)

head' :: [a] -> a
head' [] = error "Cant find head'"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num b) => [b] -> b
sum' [] = 0
sum' (h:xs) = h + sum' xs

capital :: String -> String
capital "" = "Woops empty string"
capital hehe@(x:xs) = "The whole string " ++ hehe ++ " Other bit " ++ [x]

isItHealthy :: String -> String
isItHealthy item
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = item
          skinny = "red"
          normal = "blue"
          fat = "green"

patternMatch :: String -> String -> String
patternMatch item1 item2 = "One " ++ i1 ++ "two" ++ i2
  where (i1, i2) = (item1, item2)

letBindings :: Int -> Int -> Int
letBindings a b =
  let prod = a * b
      add = a + b
  in prod + add

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
