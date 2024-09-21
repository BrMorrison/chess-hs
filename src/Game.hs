module Game where

import Data.Char

import Util

data Color = Black | White
    deriving(Eq, Show)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
    deriving(Eq, Show)
data Piece = Piece Color PieceType
    deriving(Eq, Show)

-- Define the pieces
bp :: Piece
bp = Piece Black Pawn
br :: Piece
br = Piece Black Rook
bn :: Piece
bn = Piece Black Knight
bb :: Piece
bb = Piece Black Bishop
bq :: Piece
bq = Piece Black Queen
bk :: Piece
bk = Piece Black King
wp :: Piece
wp = Piece White Pawn
wr :: Piece
wr = Piece White Rook
wn :: Piece
wn = Piece White Knight
wb :: Piece
wb = Piece White Bishop
wq :: Piece
wq = Piece White Queen
wk :: Piece
wk = Piece White King

pieceColor :: Piece -> Color
pieceColor (Piece c _) = c

pieceType :: Piece -> PieceType
pieceType (Piece _ t) = t

data BoardSquare = Empty | Occ Piece
newtype Board = Board [[BoardSquare]]
data Game = Game Board Color

getBoard :: Game -> Board
getBoard (Game b _) = b

boardAt :: Board -> Position -> Maybe Piece
boardAt (Board board) (Vec2 col row) = case board !! row !! col of
    Empty -> Nothing
    Occ p -> Just p

startingBoard :: Board
startingBoard = Board [
    map Occ [br, bn, bb, bq, bk, bb, bn, br],
    map Occ [bp, bp, bp, bp, bp, bp, bp, bp],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map Occ [wp, wp, wp, wp, wp, wp, wp, wp],
    map Occ [wr, wn, wb, wq, wk, wb, wn, wr]]

initGame :: Game
initGame = Game startingBoard White

inBounds :: Position -> Bool
inBounds (Vec2 x y) = let helper z = (z >= 0) && (z < 8) in
    helper x && helper y

showSquare :: BoardSquare -> String
showSquare Empty = "."
showSquare (Occ (Piece c t)) = case (c, t) of
    (Black, Pawn)   -> "p"
    (Black, Rook)   -> "r"
    (Black, Knight) -> "n"
    (Black, Bishop) -> "b"
    (Black, Queen)  -> "q"
    (Black, King)   -> "k"
    (White, Pawn)   -> "P"
    (White, Rook)   -> "R"
    (White, Knight) -> "N"
    (White, Bishop) -> "B"
    (White, Queen)  -> "Q"
    (White, King)   -> "K"

drawBoardLine :: [BoardSquare] -> String
drawBoardLine line = foldl (\str square -> str ++ showSquare square ++ " ") "| " line ++ "|"

instance Show Board where
    show (Board board) = unlines
        ([ "    a b c d e f g h"
        , "  +-----------------+"]
        ++ map (\(i :: Integer, row) -> let n = show (8 - i) in
            n ++ " " ++ drawBoardLine row ++ " " ++ n) (enumerate board)
        ++ ["  +-----------------+", "    a b c d e f g h"])

instance Show Game where
    show (Game board color) = show board ++ "Turn: " ++ show color

toggleColor :: Color -> Color
toggleColor Black = White
toggleColor White = Black

decodeCoord :: String -> Maybe Position
decodeCoord [colChar, rowChar] = 
    let
        col = ord colChar - ord 'a'
        row = ord '8' - ord rowChar
        pos = Vec2 col row
    in
        if inBounds pos
            then Just pos
            else Nothing
decodeCoord _ = Nothing

encodeCoord :: Position -> String 
encodeCoord pos =
    let
        colChar = chr $ ord 'a' + getX pos
        rowChar = chr $ ord '8' - getY pos
    in [colChar, rowChar]