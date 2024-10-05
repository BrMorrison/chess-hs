module Backend.Game where

import Types
import Util

------------------------------
-- Type Definitions
------------------------------

gameOver :: Game -> Bool
gameOver game = case gameState game of
    Checkmate -> True
    Stalemate -> True
    _ -> False

boardAt :: Board -> Position -> Maybe Piece
boardAt (Board board) (Vec2 col row) = case board !! row !! col of
    Empty -> Nothing
    Occ p -> Just p

setBoardAt :: Board -> Position -> BoardSquare -> Board
setBoardAt (Board b) pos newSquare = Board $ 
    map (\(rowIndex, row) -> 
        map (\(colIndex, oldSquare) ->
            if Vec2 colIndex rowIndex == pos
                then newSquare
                else oldSquare
            ) (enumerate row)
        ) (enumerate b)

movePiece :: Piece -> Move -> Board -> Board
movePiece piece (Move p1 p2) board = setBoardAt (setBoardAt board p2 (Occ piece)) p1 Empty

-- Helper function that prepends the pieces in a row with their locations to a
-- list of pieces and locations.
piecesInRow :: Int -> [BoardSquare] -> [(Piece, Position)] -> [(Piece, Position)]
piecesInRow rowNum row startingPieces =
    foldl 
        (\pieces (colNum, square) -> case square of
            Occ piece -> (piece, Vec2 colNum rowNum):pieces
            Empty -> pieces)
        startingPieces
        (enumerate row)

allPieces :: Board -> [(Piece, Position)]
allPieces (Board board) =
    foldl (\pieces (rowNum, row) -> piecesInRow rowNum row pieces)
        []
        (enumerate board)

-- Looks through all the pieces and return the location of the first
-- one that matches
pieceLocation :: Board -> Piece -> Maybe Position
pieceLocation board piece = 
    case filter ((== piece) . fst) (allPieces board) of
        ((_, pos):_) -> Just pos
        [] -> Nothing

colorPieceLocations :: Board -> Color -> [Position]
colorPieceLocations board color = map snd $ filter ((== color) . pieceColor . fst) $ allPieces board

------------------------------
-- Constant Definitions
------------------------------

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

emptyTiles :: Int -> [BoardSquare]
emptyTiles x = take x $ map (const Empty) [1 ::Integer ..]

startingBoard :: Board
startingBoard = Board [
    map Occ [br, bn, bb, bq, bk, bb, bn, br],
    map Occ [bp, bp, bp, bp, bp, bp, bp, bp],
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    map Occ [wp, wp, wp, wp, wp, wp, wp, wp],
    map Occ [wr, wn, wb, wq, wk, wb, wn, wr]]

initGame :: Game
initGame = Game { gameBoard = startingBoard
                , gameTurn = White
                , gameState = Normal
                , gameMoves = [] }
