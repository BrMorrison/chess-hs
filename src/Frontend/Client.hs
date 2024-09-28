
-- This module contains the code for interfacing with the backend/server
module Frontend.Client (evalCmd, drawGame) where

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
        (Nothing, _) -> state ("Invalid coordinate: " ++ arg1,)
        (_, Nothing) -> state ("Invalid coordinate: " ++ arg2,)
        (Just p1, Just p2) -> handleMove' (Move p1 p2)
handleMove _ = state ("Usage: move <pos1> <pos2>\n  (ex: move a1 b2)", )

handleRobot :: State Game String
handleRobot = do
    game <- get
    put $ robotMove game
    return ""

printOptions' :: Position -> State Game String
printOptions' pos =
    let moves game = validMovesAt (gameBoard game) pos
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

------------------------------
-- Drawing functions
------------------------------

showSquare :: BoardSquare -> String
showSquare Empty = "."
showSquare (Occ (Piece c t)) = case (c, t) of
    (Black, Pawn)   -> "p"
    (Black, Rook)   -> "r"
    (Black, Knight) -> "n"
    (Black, Bishop) -> "b"
    (Black, Queen)  -> "q"
    (Black, King)   -> "k"
    (White, Pawn)   -> "P"
    (White, Rook)   -> "R"
    (White, Knight) -> "N"
    (White, Bishop) -> "B"
    (White, Queen)  -> "Q"
    (White, King)   -> "K"

drawBoardLine :: [BoardSquare] -> String
drawBoardLine line = foldl (\str square -> str ++ showSquare square ++ " ") "| " line ++ "|"

drawBoard :: Board -> String
drawBoard (Board board) = unlines
    ([ "    a b c d e f g h"
    , "  +-----------------+"]
    ++ map (\(i :: Integer, row) -> let n = show (8 - i) in
        n ++ " " ++ drawBoardLine row ++ " " ++ n) (enumerate board)
    ++ ["  +-----------------+", "    a b c d e f g h"])

drawGame :: Game -> String
drawGame (Game board color gameState _) = 
    let turnStr = "Turn: " ++ show color
        message = case gameState of
            Checkmate -> "Checkmate. " ++ show (toggleColor color) ++ " wins"
            Stalemate -> "Stalemate. Game Over"
            Check -> turnStr ++ " (Check)"
            Normal -> turnStr
    in drawBoard board ++ message ++ "\n"