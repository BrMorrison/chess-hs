module Util where

import Data.Char

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

inBounds :: Position -> Bool
inBounds (Vec2 x y) = let helper z = (z >= 0) && (z < 8) in
    helper x && helper y

decodeCoord :: String -> Maybe Position
decodeCoord [colChar, rowChar] = 
    let col = ord colChar - ord 'a'
        row = ord '8' - ord rowChar
        pos = Vec2 col row
    in if inBounds pos
        then Just pos
        else Nothing
decodeCoord _ = Nothing

encodeCoord :: Position -> String 
encodeCoord pos =
    let colChar = chr $ ord 'a' + getX pos
        rowChar = chr $ ord '8' - getY pos
    in [colChar, rowChar]
