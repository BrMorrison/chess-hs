module Movement ( validMoves
                , moveValid
                , inCheck
                , inCheckmate
                , inStalemate
                , checkGameState
                , allPossibleMoves) where

import Data.Maybe
import Game
import Util

------------------------------
-- Helpers
------------------------------

friendlyCollisionAt :: Board -> Color -> Position -> Bool
friendlyCollisionAt b color pos = let collidePiece = boardAt b pos in
    case collidePiece of
        Nothing -> False
        Just (Piece collideColor _) -> color == collideColor

enemyCollisionAt :: Board -> Color -> Position -> Bool
enemyCollisionAt b color pos = let collidePiece = boardAt b pos in
    case collidePiece of
        Nothing -> False
        Just (Piece collideColor _) -> color /= collideColor

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

type MoveFunc = Board -> Color -> Position -> [Position]

movesInDirection :: Vec2 -> MoveFunc
movesInDirection dir board c pos =
    let allMoves = iterate (|+|dir) (pos|+|dir)
        notFriendlyCollision p = not (friendlyCollisionAt board c p)
        notPrevMoveCapture p = (p == pos) || not (enemyCollisionAt board c (p |-| dir))
    in takeWhile 
        (\p -> inBounds p && notFriendlyCollision p && notPrevMoveCapture p)
        allMoves

movesInDirections :: [Vec2] -> MoveFunc
movesInDirections dirs board c pos = concatMap (\dir -> movesInDirection dir board c pos) dirs

movesToSpots :: [Vec2] -> MoveFunc
movesToSpots dirs board c pos =
    let possibleMoves = map (|+| pos) dirs
    in filter (\p -> inBounds p && not (friendlyCollisionAt board c p)) possibleMoves

pieceMoveFunc :: PieceType -> MoveFunc
pieceMoveFunc p 
    | p == Pawn = pawnMoves
    | p == King || p == Knight = movesToSpots (pieceDirections p)
    | otherwise = movesInDirections (pieceDirections p)

-- Pawn movement is complicated, so it has to be handled separately.
pawnMoves :: MoveFunc
pawnMoves board color pos = 
    let dir = if color == Black then Vec2 0 1 else Vec2 0 (-1)
        onStartingSquare = (color == Black && getY pos == 1) || (color == White && getY pos == 6)
        forwardPos = pos |+| dir
        baseMoveOptions = forwardPos : [forwardPos |+| dir | onStartingSquare]
        validMoveOptions = takeWhile 
            (\p -> inBounds p && isNothing (boardAt board p))
            baseMoveOptions
        baseCaptureOptions = [forwardPos |+| Vec2 1 0, forwardPos |+| Vec2 (-1) 0]
        validCaptureOptions = filter (\p -> inBounds p && enemyCollisionAt board color p) baseCaptureOptions
    in validMoveOptions ++ validCaptureOptions

validMoves' :: Board -> Position -> [Position]
validMoves' board pos = case boardAt board pos of
    Nothing -> []
    Just (Piece c p) -> pieceMoveFunc p board c pos

validMoves :: Board -> Position -> [Position]
validMoves board pos = case boardAt board pos of
    Nothing -> []
    Just piece ->
        let color = pieceColor piece
            baseMoves = validMoves' board pos
            avoidsCheck newPos = not $ inCheck color (movePiece (Move pos newPos) board)
        in filter avoidsCheck baseMoves

moveValid :: Color -> Board -> Move -> Bool
moveValid color board (Move pStart pEnd) = 
    let rightColor = case boardAt board pStart of
            Just piece -> pieceColor piece == color
            Nothing -> False
        in rightColor && elem pEnd (validMoves board pStart)


-- enumerate all possible next board states for a given color
allPossibleMoves :: Color -> Board -> [Move]
allPossibleMoves color board =
    let piecesLocations = colorPieceLocations board color
        movesForPiece loc = map (Move loc) (validMoves board loc)
    in foldl (\moves loc -> moves ++ movesForPiece loc) [] piecesLocations

possibleNextBoards :: Color -> Board -> [Board]
possibleNextBoards color board = map (`movePiece` board) (allPossibleMoves color board)

------------------------------
-- Check Code
------------------------------

inCheck :: Color -> Board -> Bool
inCheck color board =
    let kingPos = fromJust $ pieceLocation board (Piece color King)
        enemyPieceLocations = colorPieceLocations board (toggleColor color)
        -- Note, we use validMoves' because it doesn't include the check check.
        enemyMoves = foldl (\moves pos -> moves ++ validMoves' board pos) [] enemyPieceLocations
    in elem kingPos enemyMoves

inCheckmate :: Color -> Board -> Bool
inCheckmate color board = inCheck color board && all (inCheck color) (possibleNextBoards color board)

inStalemate :: Color -> Board -> Bool
inStalemate color board = case allPossibleMoves color board of
    [] -> True
    _ -> False

checkGameState :: Color -> Board -> GameState
checkGameState nextColor board =
    let checkmate = inCheckmate nextColor board
        stalemate = inStalemate nextColor board
        check = inCheck nextColor board
    in case (checkmate, stalemate, check) of
        (True,  _,     _    ) -> Checkmate
        (False, True,  _    ) -> Stalemate
        (False, False, True ) -> Check
        (False, False, False) -> Normal