module Game where

import Util

------------------------------
-- Type Definitions
------------------------------

data Color = Black | White
    deriving(Eq, Show)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
    deriving(Eq, Show)
data Piece = Piece Color PieceType
    deriving(Eq, Show)
data BoardSquare = Empty | Occ Piece
-- This could probably be refactored into a Seq or something to improve performance
newtype Board = Board [[BoardSquare]]
data GameState = Normal | Check | Checkmate | Stalemate
data Move = Move Position Position
data Game = Game Board Color GameState [GameMove]

data Annotation = Brilliant | Good | Bad | Blunder

data GameMove = GameMove 
    { gameMovePiece      :: PieceType
    , gameMoveCapture    :: Bool
    , gameMoveMove       :: Move
    , gameMovePromotion  :: Maybe PieceType
    , gameMoveState      :: GameState
    , gameMoveAnnotation :: Maybe Annotation }

pieceColor :: Piece -> Color
pieceColor (Piece c _) = c

toggleColor :: Color -> Color
toggleColor Black = White
toggleColor White = Black

pieceType :: Piece -> PieceType
pieceType (Piece _ t) = t

getBoard :: Game -> Board
getBoard (Game b _ _ _) = b

commitMove :: GameMove -> Board -> GameState -> Game -> Game
commitMove move nextBoard nextState (Game _ turn _ moves) = 
    Game nextBoard (toggleColor turn) nextState (move:moves)

getTurn :: Game -> Color
getTurn (Game _ c _ _) = c

getState :: Game -> GameState
getState (Game _ _ state _) = state

gameOver :: Game -> Bool
gameOver (Game _ _ state _) = case state of
    Checkmate -> True
    Stalemate -> True
    _ -> False

moveOrig :: Move -> Position
moveOrig (Move p _) = p

moveDest :: Move -> Position
moveDest (Move _  p) = p

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
initGame = Game startingBoard White Normal []

------------------------------
-- Drawing functions
------------------------------

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
    show (Game board color gameState _) = 
        let turnStr = "Turn: " ++ show color
            message = case gameState of
                Checkmate -> "Checkmate. " ++ show (toggleColor color) ++ " wins"
                Stalemate -> "Stalemate. Game Over"
                Check -> turnStr ++ " (Check)"
                Normal -> turnStr
        in show board ++ message ++ "\n"
