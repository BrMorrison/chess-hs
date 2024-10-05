
-- This module contains the code for interfacing with the backend/server
module Frontend.Client (evalCmd) where

import Control.Monad.Trans.State

import Types
import Util
import Backend.Interface

handleMove' :: Move -> State Game String
handleMove' move = do
        game <- get
        case makeMove move game of
            Just game' -> put game' >> return ""
            Nothing -> return "Move not valid"

handleMove :: [String] -> State Game String
handleMove [arg1, arg2] = 
    case (decodeCoord arg1, decodeCoord arg2) of
        (Nothing, _) -> state (\game -> ("Invalid coordinate: " ++ arg1, game))
        (_, Nothing) -> state (\game -> ("Invalid coordinate: " ++ arg2, game))
        (Just p1, Just p2) -> handleMove' (Move p1 p2)
handleMove _ = state (\game -> ("Usage: move <pos1> <pos2>\n  (ex: move a1 b2)", game))

handleRobot :: State Game String
handleRobot = do
    game <- get
    put $ robotMove game
    return ""

printOptions' :: Position -> State Game String
printOptions' pos =
    let moves game = validMovesAt game pos
    in (\coords -> "[" ++ unwords coords ++ "]" ) . map encodeCoord . moves <$> get

printOptions :: [String] -> State Game String
printOptions [coord] = 
    case decodeCoord coord of
        Nothing -> return $ "Invalid coordinate: " ++ coord
        Just pos -> printOptions' pos
printOptions _ = return "Usage: options <pos>"

helpMsg :: String
helpMsg = unlines
    [ "Commands"
    , "  move <c1> <c2>: Try to move the piece from <c1> to <c2>"
    , "  options <c>:    Print the move options for the piece at <c>"
    , "  help:           Print this help message"
    , "  robot:          Let the chess AI make a move"
    , "  quit:           End the game."]

evalCmd :: [String] -> State Game String
evalCmd ("move": args) = handleMove args
evalCmd ("options": args) = printOptions args
evalCmd ("help": _) = return helpMsg
evalCmd ("robot": _) = handleRobot
evalCmd (cmd:_) = return $ "Unrecognized command: " ++ cmd
evalCmd [] = return ""
