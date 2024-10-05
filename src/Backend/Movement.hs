module Backend.Movement
    ( validMovesAt
    , contextFreeValidMovesAt
    , checkGameState
    , allPossibleMoves
    , makeMove
    ) where

import Data.Maybe

import Types
import Util
import Backend.Game

------------------------------
-- Collision Helpers
------------------------------

friendlyCollisionAt :: Color -> Board -> Position -> Bool
friendlyCollisionAt color board pos = fmap pieceColor (boardAt board pos) == Just color

enemyCollisionAt :: Color -> Board -> Position -> Bool
enemyCollisionAt color board pos = fmap pieceColor (boardAt board pos) == Just (toggleColor color)

anyCollisionAt :: Board -> Position -> Bool
anyCollisionAt board pos = isJust $ boardAt board pos

------------------------------
-- Piece Direction Definitions
------------------------------

pieceDirections :: PieceType -> [Vec2]
pieceDirections Rook = [Vec2 1 0, Vec2 (-1) 0, Vec2 0 (-1), Vec2 0 1]
pieceDirections Knight = [Vec2 1 (-2), Vec2 2 (-1), Vec2 2 1, Vec2 1 2,
                    Vec2 (-1) (-2), Vec2 (-2) (-1), Vec2 (-2) 1, Vec2 (-1) 2]
pieceDirections Bishop = [Vec2 1 1, Vec2 (-1) 1, Vec2 (-1) (-1), Vec2 1 (-1)]
pieceDirections Queen = pieceDirections Rook ++ pieceDirections Bishop
pieceDirections King = pieceDirections Queen
pieceDirections Pawn = [] -- Not used for Pawns

------------------------------
-- Movement functions
------------------------------

-- Type alias for a function that enumerates possible moves for a piece
type MoveFunc = Color -> Board -> Position -> [Position]

-- Movement helper that enumerates the possible moves in a straight line (for pieces like rooks, 
-- bishops, and queens). The movement continues until the line encounters another piece or the end
-- of the board.
movesInDirection' :: Vec2 -> MoveFunc
movesInDirection' dir _ board pos =
    takeWhile (\p -> inBounds p && not (anyCollisionAt board p)) allMoves
    where allMoves = iterate (|+|dir) (pos|+|dir)

-- Like movesInDirection', but allows a capture at the end of the move
movesInDirection :: Vec2 -> MoveFunc
movesInDirection dir color board pos =
    if inBounds nextMove && enemyCollisionAt color board nextMove
        then moves ++ [nextMove]
        else moves
    where
        moves = movesInDirection' dir color board pos
        lastMove = if null moves
            then pos
            else last moves
        nextMove = lastMove |+| dir

movesInDirections :: [Vec2] -> MoveFunc
movesInDirections dirs color board pos = concatMap (\dir -> movesInDirection dir color board pos) dirs

-- Similar to movesInDirection, but only checks discrete points. Used for checking knight and king
-- movement.
movesToSpots :: [Vec2] -> MoveFunc
movesToSpots dirs color board pos =
    filter (\p -> inBounds p && not (friendlyCollisionAt color board p)) possibleMoves
    where possibleMoves = map (|+| pos) dirs

-- Pawn movement is complicated, so it has to be handled separately.
pawnMoves :: MoveFunc
pawnMoves color board pos = validMoveOptions ++ validCaptureOptions
    where
        dir = if color == Black
            then Vec2 0 1
            else Vec2 0 (-1)
        onStartingSquare = case color of
            Black -> getY pos == 1
            White -> getY pos == 6
        forwardPos = pos |+| dir
        baseMoveOptions = if onStartingSquare
            then [forwardPos, forwardPos |+| dir]
            else [forwardPos]
        -- Use movesInDirection to filter out moves that would result in collisions
        validMoveOptions = filter (`elem` movesInDirection' dir color board pos) baseMoveOptions
        baseCaptureOptions = [forwardPos |+| left, forwardPos |+| right]
            where left = Vec2 (-1) 0; right = Vec2 1 0
        validCaptureOptions = filter (\p -> inBounds p && enemyCollisionAt color board p) baseCaptureOptions

pieceMoveFunc :: PieceType -> MoveFunc
pieceMoveFunc p 
    | p == Pawn = pawnMoves
    | p == King || p == Knight = movesToSpots (pieceDirections p)
    | otherwise = movesInDirections (pieceDirections p)

-- Gets the moves for the piece at the given position, not including context-sensitive moves like
-- castling and en-passant
contextFreeValidMovesAt :: Board -> Position -> [Position]
contextFreeValidMovesAt board pos = do
    (Piece pColor pType) <- maybeToList (boardAt board pos)
    pieceMoveFunc pType pColor board pos

-- Gets the valid moves for a piece at the given position.
-- Note: Doesn't consider whether a move puts oneself in check.
validMovesAt :: Game -> Position -> [Position]
validMovesAt game = contextFreeValidMovesAt (gameBoard game)

-- enumerate all possible next board states for a given color
allPossibleMoves :: Game -> [Move]
allPossibleMoves game = let 
    piecesLocations = colorPieceLocations (gameBoard game) (gameTurn game)
    movesForPiece loc = map (Move loc) (validMovesAt game loc)
    allMoves = foldl (\moves loc -> moves ++ movesForPiece loc) [] piecesLocations
    in filter (validateMove game) allMoves

possibleNextBoards :: Game -> [Board]
possibleNextBoards game = map (gameBoard . fromJust) possibleNextGames
    where
        allNextGames = map (`makeMove` game) (allPossibleMoves game)
        possibleNextGames = filter isJust allNextGames

isMovePawnPromotion :: Piece -> Move -> Bool
isMovePawnPromotion piece move =
    case (pieceType piece, pieceColor piece, (getY . moveDest) move) of
        (Pawn, Black, 7) -> True
        (Pawn, White, 0) -> True
        _ -> False

validateMove :: Game -> Move -> Bool
validateMove game move = isRightColor && destValid && avoidsCheck
    where
        color = gameTurn game
        board = gameBoard game
        destValid = moveDest move `elem` validMovesAt game (moveOrig move)
        maybePiece = boardAt (gameBoard game) (moveOrig move)
        isRightColor = maybe False (\piece -> pieceColor piece == color) maybePiece
        avoidsCheck = not $ inCheck color (movePiece (fromJust maybePiece) move board)

commitMove :: Move -> Board -> Game -> Game
commitMove move nextBoard (Game _ turn st moves) = let
    -- TODO: Another place that really could use lenses
    tmpGame = Game nextBoard (toggleColor turn) st (move:moves)
    nextSt = checkGameState tmpGame
    in Game 
        { gameBoard = gameBoard tmpGame
        , gameTurn = gameTurn tmpGame 
        , gameState = nextSt
        , gameMoves = gameMoves tmpGame
        }

makeMove' :: Move -> Game -> Piece -> Game
makeMove' move game piece = commitMove move nextBoard game
    where 
        color = gameTurn game
        board = gameBoard game
        finalPiece = if isMovePawnPromotion piece move
            then Piece color Queen
            else piece
        nextBoard = movePiece finalPiece move board

makeMove :: Move -> Game -> Maybe Game
makeMove move game =
    if not $ validateMove game move
        then Nothing
        else do 
            piece <- boardAt (gameBoard game) (moveOrig move)
            return $ makeMove' move game piece

------------------------------
-- Check Code
------------------------------

inCheck :: Color -> Board -> Bool
inCheck color board = case pieceLocation board (Piece color King) of
    -- If there's no king, then it's not in check (really only used for tests)
    Nothing -> False
    Just kingPos -> kingPos `elem` enemyMoves
        where
            enemyPieceLocations = colorPieceLocations board (toggleColor color)
            -- We can use `contextFreeValidMovesAt` here since context-sensitive moves like
            -- castling and en-passant won't threaten the king.
            enemyMoves = concatMap (contextFreeValidMovesAt board) enemyPieceLocations

inCheckmate :: Game -> Bool
inCheckmate game = currentlyInCheck && all (inCheck color) nextBoards
    where
        color = gameTurn game
        currentlyInCheck = inCheck color (gameBoard game)
        nextBoards = possibleNextBoards game

inStalemate :: Game -> Bool
inStalemate game = null $ allPossibleMoves game

checkGameState :: Game -> GameState
checkGameState game =
    let
        checkmate = inCheckmate game
        stalemate = inStalemate game
        check = inCheck (gameTurn game) (gameBoard game)
    in case (checkmate, stalemate, check) of
        (True,  _,     _    ) -> Checkmate
        (False, True,  _    ) -> Stalemate
        (False, False, True ) -> Check
        (False, False, False) -> Normal
