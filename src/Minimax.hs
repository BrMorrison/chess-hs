module Minimax where

import Game
import Movement

materialWorth' :: PieceType -> Int
materialWorth' Pawn = 1
materialWorth' Rook = 5
materialWorth' Knight = 3
materialWorth' Bishop = 3
materialWorth' Queen = 9
materialWorth' King = 0

materialWorth :: Piece -> Int
materialWorth (Piece color piece) = case color of
    White -> materialWorth' piece
    Black -> negate(materialWorth' piece)

pieceCounts :: [(PieceType, Int)]
pieceCounts = [(Pawn, 8), (Rook, 2), (Knight, 2), (Bishop, 2), (Queen, 1)]

-- Provides a score from [-1, 1] based purely on the number of pieces
-- each side has. White is positive.
materialScore :: Board -> Double
materialScore board =
    let maxScore = sum $ map
            (\(piece, count) -> materialWorth' piece * count)
            pieceCounts
        score = sum $ map (materialWorth . fst) (allPieces board)
    in fromIntegral score / fromIntegral maxScore

scoreBoard :: Board -> Double
scoreBoard board
    | inCheckmate Black board = 1.0
    | inCheckmate White board = -1.0
    | inStalemate Black board || inStalemate White board = 0.0
    | otherwise = materialScore board

getBestScore :: Color -> [Double] -> Double
getBestScore White = foldl max (-1.0)
getBestScore Black = foldl min 1.0

miniMax' :: Int -> Color -> Board -> (Maybe Move, Double)
miniMax' 0 _ board = (Nothing, scoreBoard board)
miniMax' fuel color board =
    let moves = allPossibleMoves color board
        nextMiniMaxScore = snd . miniMax' (fuel-1) (toggleColor color)
        moveScores = map (\move -> (move, nextMiniMaxScore (movePiece move board))) moves
        bestScore = getBestScore color (map snd moveScores)
        -- Ideally, we'd pick one of the valid moves randomly instead of just grabbing the first one
        bestMove = fst . head $ filter ((== bestScore) . snd) moveScores
    in case moves of
        [] -> (Nothing, scoreBoard board) -- Handle the case where we don't have moves available
        _ -> (Just bestMove, bestScore)

miniMax :: Color -> Board -> (Maybe Move, Double)
miniMax = miniMax' 3 