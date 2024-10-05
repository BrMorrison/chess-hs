
module Backend.Interface 
    ( module Backend.Interface
    , makeMove
    , validMovesAt
    ) where

import Data.Maybe (fromMaybe)
import Types
import Backend.Game (initGame)
import Backend.Movement (makeMove, validMovesAt)
import Backend.Minimax (miniMax)

newGame :: Game
newGame = initGame

getPastMoves :: Game -> [Move]
getPastMoves = gameMoves
 
-- gameProbability :: ChessGame -> Double

robotMove :: Game -> Game
robotMove game = fromMaybe
    (error "The AI couldn't find any valid moves.")
    $ do
        move <- (fst . miniMax) game
        makeMove move game