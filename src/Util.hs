module Util where

data Vec2 = Vec2 Int Int
    deriving(Eq, Show)

type Position = Vec2
(|+|) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) |+| (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
(|-|) :: Vec2 -> Vec2 -> Vec2
(Vec2 x1 y1) |-| (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

getX :: Vec2 -> Int
getX (Vec2 x _) = x
getY :: Vec2 -> Int
getY (Vec2 _ y) = y

enumerate :: Integral a => [b] -> [(a, b)]
enumerate = zip [0..]
