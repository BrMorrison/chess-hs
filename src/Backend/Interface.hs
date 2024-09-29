
module Backend.Interface 
    ( module Backend.Interface
    , makeMove
    , validMovesAt
    ) where

import Types
import Backend.Game (initGame)
import Backend.Movement (allPossibleMoves, makeMove, validMovesAt)
import Backend.Minimax (miniMax)

newGame :: Game
newGame = initGame

-- Returns a list of valid moves for the current state of the game.
getValidMoves :: Game -> [Move]
getValidMoves game = allPossibleMoves (gameTurn game) (gameBoard game)

-- Tries to make the specified move
-- Returns the updated game state if the move is valid and Nothing if it's not.
-- makeMove :: Move -> Game -> Maybe Game
-- makeMove move game

getPastMoves :: Game -> [GameMove]
getPastMoves = gameMoves
 
-- gameProbability :: ChessGame -> Double

robotMove :: Game -> Game
robotMove game = maybe 
    (error "The AI couldn't find any valid moves.")
    id $ do
        move <- (fst . miniMax) game
        makeMove move game