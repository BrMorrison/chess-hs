module Repl where

import Control.Monad
import Data.Functor
import System.IO
import Control.Monad.Trans.State

import Game
import Util
import Movement


------------------------------------------
-- Movement Code
------------------------------------------

movePieceHelper :: Board -> Position -> Position -> Position -> BoardSquare
movePieceHelper (Board b) p1 p2 curPos
    | p1 == curPos = Empty
    | p2 == curPos = b !! getY p1 !! getX p1
    | otherwise = b !! getY curPos !! getX curPos

-- Helper function that moves pieces without validation
movePiece' :: Board -> Position -> Position -> Board
movePiece' (Board b) p1 p2 = Board $ 
    map (\(rowIndex, row) -> 
        map (\(colIndex, _) ->
            movePieceHelper (Board b) p1 p2 (Vec2 colIndex rowIndex)
            ) (enumerate row)
        ) (enumerate b)

movePiece :: Position -> Position -> State Game (Either () String)
movePiece p1 p2 = do 
    (Game board color) <- get
    let
        rightColor = case boardAt board p1 of
            Just piece -> pieceColor piece == color
            Nothing -> False
        moveValid = elem p2 (validMoves board p1) && rightColor
        in if moveValid
            then put (Game (movePiece' board p1 p2) (toggleColor color)) >> return (Left ())
            else return $ Right "Move not valid"

------------------------------------------
-- REPL Code
------------------------------------------
handleMove' :: Position -> Position -> State Game String
handleMove' p1 p2 =
    let
        result = movePiece p1 p2
    in do
    game <- get
    case runState result game of
        (Right msg, _) -> return msg
        (Left (), game') -> put game' >> return (show game')

handleMove :: [String] -> State Game String
handleMove [arg1, arg2] = 
    case (decodeCoord arg1, decodeCoord arg2) of
        (Nothing, _) -> state ("Invalid coordinate: " ++ arg1,)
        (_, Nothing) -> state ("Invalid coordinate: " ++ arg2,)
        (Just p1, Just p2) -> handleMove' p1 p2
handleMove _ = state ("Usage: move <pos1> <pos2>", )


printOptions' :: Position -> State Game String
printOptions' pos =
    let
        moves game = validMoves (getBoard game) pos
    in (\coords -> "[" ++ unwords coords ++ "]" ) . map encodeCoord . moves <$> get

printOptions :: [String] -> State Game String
printOptions [coord] = 
    case decodeCoord coord of
        Nothing -> return $ "Invalid coordinate: " ++ coord
        Just pos -> printOptions' pos
printOptions _ = return "Usage: options <pos>"

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
    , "  quit:           End the game."]

eval' :: [String] -> State Game String
eval' ("print": _) = get <&> show
eval' ("move": args) = handleMove args
eval' ("options": args) = printOptions args
eval' ("help": _) = return helpMsg
eval' (cmd:_) = return $ "Unrecognized command: " ++ cmd
eval' [] = return ""


repl :: Game -> IO ()
repl game = do
    input <- read'
    unless (input == "quit")
            $ (let (msg, game') = runState (eval' (words input)) game
            in putStrLn msg >> repl game')
