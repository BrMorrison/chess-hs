module Backend.Minimax
    ( miniMax
    , scoreBoard
    , materialScore
    , positionScore
    ) where

import Data.Maybe

import Types
import Util

import Backend.Game
import Backend.Movement

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

maxMaterialScore :: Int
maxMaterialScore = sum $ map
        (\(piece, count) -> materialWorth' piece * count)
        pieceCounts

-- Provides a score from [-1, 1] based purely on the number of pieces
-- each side has. White is positive.
materialScore :: Board -> Double
materialScore board =
    let score = sum $ map (materialWorth . fst) (allPieces board)
    in fromIntegral score / fromIntegral maxMaterialScore

-- The furthest you can be from a center tile is 3 squares in both directions.
maxDistanceFromCenter :: Double
maxDistanceFromCenter = 3.0 * sqrt 2

distanceFromCenter :: Vec2 -> Double
distanceFromCenter pos =
    let centerTiles = [Vec2 3 3, Vec2 4 3, Vec2 3 4, Vec2 4 4]
        distances = map (distance pos) centerTiles
    in foldl min maxDistanceFromCenter distances

positionWorth :: Piece -> Vec2 -> Double
positionWorth piece pos =
    let posDistance = distanceFromCenter pos
        -- The best score would be if you had every piece on a center tile
        -- (even though that's not possible)
        normalizeScore score = score / fromIntegral maxMaterialScore
        baseScore = (1.0 - (posDistance/maxDistanceFromCenter)) * fromIntegral (materialWorth piece)
    in normalizeScore baseScore

-- Provides a score from roughly (-1, 1) based on the position of the pieces.
-- Having higher value pieces near the board is worth more points.
positionScore :: Board -> Double
positionScore board = 
    let scores = map (uncurry positionWorth) (allPieces board)
    in sum scores

-- The value is based on the material score and the position score with some weighting
scoreBoard' :: Board -> Double
scoreBoard' board = 0.9 * materialScore board
                  + 0.1 * positionScore board

scoreBoard :: Board -> Double
scoreBoard board
    | inCheckmate Black board = 1.0
    | inCheckmate White board = -1.0
    | inStalemate Black board || inStalemate White board = 0.0
    | otherwise = scoreBoard' board 

getBestScore :: Color -> [Double] -> Double
getBestScore White = foldl max (-1.0)
getBestScore Black = foldl min 1.0

-- TODO: We should have this return a tree or something so that we don't need to recalculate it each time.
miniMax' :: Int -> Game -> (Maybe Move, Double)
miniMax' 0 game = (Nothing, scoreBoard (gameBoard game))
miniMax' fuel game =
    let color = gameTurn game
        board = gameBoard game
        moves = allPossibleMoves color board
        nextGame move = fromJust (makeMove move game)
        nextMiniMaxScore = snd . miniMax' (fuel-1)
        moveScores = map (\move -> (move, nextMiniMaxScore (nextGame move))) moves
        bestScore = getBestScore color (map snd moveScores)
        -- Ideally, we'd pick one of the valid moves randomly instead of just grabbing the first one
        bestMove = fst . head $ filter ((== bestScore) . snd) moveScores
    in case moves of
        [] -> (Nothing, scoreBoard board) -- Handle the case where we don't have moves available
        _ -> (Just bestMove, bestScore * 0.999) -- The * 0.999 is so that we favor shorter trees

miniMax :: Game -> (Maybe Move, Double)
miniMax = miniMax' 3 