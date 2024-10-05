module Types where

-- Type for representing 2D directions and positions on the board.
data Vec2 = Vec2 Int Int
    deriving(Eq, Show, Ord)
type Position = Vec2

data Color = Black | White
    deriving(Eq, Show)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
    deriving(Eq, Show)
data Piece = Piece Color PieceType
    deriving(Eq, Show)

data Move = Move Position Position
    deriving(Eq, Show)

-- BoardSquare and Board are used to represent the board of the game
-- Note: These should be optimized in the future to be more efficient.
data BoardSquare = Empty | Occ Piece
    deriving(Eq, Show)
newtype Board = Board [[BoardSquare]]
    deriving(Eq, Show)

-- Game should represent everything needed to represent a game of chess
data GameState = Normal | Check | Checkmate | Stalemate
    deriving(Eq, Show)
data Game = Game { gameBoard :: Board
                 , gameTurn  :: Color
                 , gameState :: GameState 
                 , gameMoves :: [Move] }
    deriving(Eq, Show)

pieceColor :: Piece -> Color
pieceColor (Piece c _) = c

toggleColor :: Color -> Color
toggleColor Black = White
toggleColor White = Black

pieceType :: Piece -> PieceType
pieceType (Piece _ t) = t

moveOrig :: Move -> Position
moveOrig (Move p _) = p

moveDest :: Move -> Position
moveDest (Move _  p) = p
