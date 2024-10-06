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

-- GameMove captures everything that's needed for a single move in algebraic notation.
data Move = Move Position Position
    deriving(Eq, Show)
data Annotation = Brilliant | Good | Bad | Blunder
    deriving(Eq, Show)
data GameMove = GameMove 
    { gameMovePiece      :: PieceType
    , gameMoveCapture    :: Bool
    , gameMoveMove       :: Move
    , gameMovePromotion  :: Maybe PieceType
    , gameMoveState      :: GameState
    , gameMoveAnnotation :: Maybe Annotation }

-- BoardSquare and Board are used to represent the board of the game
-- Note: These should be optimized in the future to be more efficient.
data BoardSquare = Empty | Occ Piece
newtype Board = Board [[BoardSquare]]

-- Game should represent everything needed to represent a game of chess
data GameState = Normal | Check | Checkmate | Stalemate
data Game = Game { gameBoard :: Board
                 , gameTurn :: Color
                 , gameState :: GameState 
                 , gameMoves :: [GameMove] }

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
