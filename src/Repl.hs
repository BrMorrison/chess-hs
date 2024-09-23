module Repl where

import Control.Monad
import Data.Functor
import System.IO
import Control.Monad.Trans.State

import Game
import Util
import Movement
import Minimax

------------------------------------------
-- REPL Code
------------------------------------------

handleMove' :: Move -> State Game String
handleMove' move = do
        game <- get
        case makeMove move game of
            Just game' -> put game' >> return (show game')
            Nothing -> return "Move not valid"

handleMove :: [String] -> State Game String
handleMove [arg1, arg2] = 
    case (decodeCoord arg1, decodeCoord arg2) of
        (Nothing, _) -> state ("Invalid coordinate: " ++ arg1,)
        (_, Nothing) -> state ("Invalid coordinate: " ++ arg2,)
        (Just p1, Just p2) -> handleMove' (Move p1 p2)
handleMove _ = state ("Usage: move <pos1> <pos2>\n  (ex: move a1 b2)", )

handleRobot :: State Game String
handleRobot = do
    game <- get
    case miniMax game of
        (Nothing, _) -> state ("The AI couldn't find any valid moves. Seems like a bug :(",)
        (Just move, score) -> do
            msg <- handleMove' move
            return $ msg ++ "AI Calculated score: " ++ show score

printOptions' :: Position -> State Game String
printOptions' pos =
    let moves game = validMoves (getBoard game) pos
    in (\coords -> "[" ++ unwords coords ++ "]" ) . map encodeCoord . moves <$> get

printOptions :: [String] -> State Game String
printOptions [coord] = 
    case decodeCoord coord of
        Nothing -> return $ "Invalid coordinate: " ++ coord
        Just pos -> printOptions' pos
printOptions _ = return "Usage: options <pos>"

handleShowScore :: State Game String
handleShowScore = do
    game <- get
    let board = getBoard game
        mScore = materialScore board
        pScore = positionScore board
        score = scoreBoard board
        message = unlines [ "  --- Scoring ---"
                          , "  Material: " ++ show mScore
                          , "  Position: " ++ show pScore
                          , "  Total:    " ++ show score]
        in return message

read' :: IO String
read' = putStr ">"
    >> hFlush stdout
    >> getLine

helpMsg :: String
helpMsg = unlines
    [ "Commands"
    , "  print:          Show the game board"
    , "  move <c1> <c2>: Try to move the piece from <c1> to <c2>"
    , "  options <c>:    Print the move options for the piece at <c>"
    , "  help:           Print this help message"
    , "  robot:          Let the chess AI make a move"
    , "  quit:           End the game."]

eval' :: [String] -> State Game String
eval' ("print": _) = get <&> show
eval' ("move": args) = handleMove args
eval' ("options": args) = printOptions args
eval' ("help": _) = return helpMsg
eval' ("robot": _) = handleRobot
eval' ("score": _) = handleShowScore
eval' (cmd:_) = return $ "Unrecognized command: " ++ cmd
eval' [] = return ""

repl :: Game -> IO ()
repl game =
    if gameOver game
        then return ()
        else do
            input <- read'
            unless (input == "quit")
                    (let (msg, game') = runState (eval' (words input)) game
                    in putStrLn msg >> repl game')
