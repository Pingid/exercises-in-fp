module Shapes
( Point(..)
, Shape(..)
, surface
, circumference
, nudge
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Square Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Square (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

circumference :: Shape -> Float
circumference (Circle _ r) = 2 * pi * r
circumference (Square (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) + (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Square (Point x1 y1) (Point x2 y2)) a b = Square (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))


data Bicycle = Bicycle { brand :: String, size :: String, coolness :: Float } deriving (Show)

downRate :: Bicycle -> Bicycle
downRate (Bicycle b s c) = Bicycle b s (c - 1)
