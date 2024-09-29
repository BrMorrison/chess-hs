{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- This module contains the TUI code for running the chess program
module Frontend.Ui (uiMain) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Table
import qualified Brick.Widgets.Edit as E

import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Control.Monad.Trans.State (runState)
import Data.Text (Text)

import Types
import Frontend.Client (evalCmd)
import Backend.Interface (newGame, getPastMoves)
import Util (encodeCoord, enumerate)

newtype Name = Name ()
    deriving(Eq, Ord, Show)

data St = St { _stGame :: Game
             , _stMsg :: String
             , _stInput :: E.Editor String Name
             }
makeLenses ''St

drawEditor :: Int -> St -> Widget Name
drawEditor width st = 
    let e = E.renderEditor (str . unlines) True (st ^. stInput)
    in (str ">" <+> hLimit (width-1) e)

drawMessage :: Int -> St -> Widget Name
drawMessage width st = hLimit width $ vBox [fill ' ', str (st ^. stMsg)]

draw :: St -> [Widget Name]
draw st = [ui]
    where
        -- TODO: Cleanup magic numbers
        widgetEffects = hCenter . border -- Include the border for debugging
        textArea width = vLimit 8 (drawMessage width st
            <=> drawEditor width st)
        gameArea = vLimit 21 (drawFancyBoard st <+> vBorder <+> drawMoves 40 st)
        ui = center $ widgetEffects gameArea
            <=> widgetEffects (textArea 80)

-- Send the commands to the editor that should clear it
-- Note: I don't understand lenses well enough to really get how this works.
clearEditorInput :: EventM Name St ()
clearEditorInput = do
    zoom stInput $ E.handleEditorEvent (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl]))
    zoom stInput $ E.handleEditorEvent (VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl]))

getCommand :: EventM Name St [String]
getCommand = do
    ed <- use stInput
    return $ (words . head . E.getEditContents) ed

handleCommand :: [String] -> EventM Name St ()
handleCommand ["quit"] = halt
handleCommand cmd = do
    game <- use stGame
    let (msg, game') = runState (evalCmd cmd) game in do
        stMsg .= msg
        stGame .= game'
        return ()

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    cmd <- getCommand
    clearEditorInput
    handleCommand cmd
handleEvent ev = zoom stInput $ E.handleEditorEvent ev

theMap :: AttrMap
theMap = attrMap V.defAttr []

initState :: St
initState = St { _stGame = newGame
               , _stMsg = ""
               , _stInput = E.editor (Name ()) (Just 1) ""
               }

app :: App St e Name
app = App { appDraw = draw
          , appHandleEvent = handleEvent
          , appChooseCursor = showFirstCursor
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

uiMain :: IO ()
uiMain = do
    _ <- defaultMain app initState
    return ()

-----------------------------------
-- Code for drawing Chess Boards
-----------------------------------
drawSquare :: BoardSquare -> Text
drawSquare Empty = "   "
drawSquare (Occ (Piece c t)) = case (c, t) of
    (Black, Pawn)   -> " ♟ "
    (Black, Rook)   -> " ♜ "
    (Black, Knight) -> " ♞ "
    (Black, Bishop) -> " ♝ "
    (Black, Queen)  -> " ♛ "
    (Black, King)   -> " ♚ "
    (White, Pawn)   -> " ♙ "
    (White, Rook)   -> " ♖ "
    (White, Knight) -> " ♘ "
    (White, Bishop) -> " ♗ "
    (White, Queen)  -> " ♕ "
    (White, King)   -> " ♔ "

boardToTable :: Board -> Table Name
boardToTable (Board board) =
    setDefaultColAlignment AlignCenter $
    setDefaultRowAlignment AlignMiddle $
    table (map (map (txt . drawSquare)) board)

drawBoardTable :: St -> Widget Name 
drawBoardTable st = renderTable (boardToTable (gameBoard (st ^. stGame)))

drawFancyBoard :: St -> Widget Name
drawFancyBoard st = ranks
                <+> (files <=> drawBoardTable st <=> files)
                <+> ranks
                <=> drawGameMessage (st ^. stGame)

drawGameMessage :: Game -> Widget Name
drawGameMessage (Game _ color gameState _) = 
    let turnStr = "Turn: " ++ show color
        message = case gameState of
            Checkmate -> "Checkmate. " ++ show (toggleColor color) ++ " wins"
            Stalemate -> "Stalemate. Game Over"
            Check -> turnStr ++ " (Check)"
            Normal -> turnStr
    in str $ '\n':message

noBorders :: Table Name -> Table Name
noBorders tbl = surroundingBorder False $ rowBorders False $ columnBorders False tbl

-- TODO: Make this not take up so many lines
ranks :: Widget Name
ranks = renderTable (noBorders $
    table [ [txt "   "]
          , [txt "   "]
          , [txt " 8 "]
          , [txt "   "]
          , [txt " 7 "]
          , [txt "   "]
          , [txt " 6 "]
          , [txt "   "]
          , [txt " 5 "]
          , [txt "   "]
          , [txt " 4 "]
          , [txt "   "]
          , [txt " 3 "]
          , [txt "   "]
          , [txt " 2 "]
          , [txt "   "]
          , [txt " 1 "] ])

files :: Widget Name
files = renderTable (noBorders $
    table [map txt ["  a ", "  b ", "  c ", "  d ", "  e ", "  f ", "  g ", "  h "]])

pieceChar :: PieceType -> Char
pieceChar Pawn = 'P'
pieceChar Rook = 'R'
pieceChar Knight = 'N'
pieceChar Bishop = 'B'
pieceChar Queen = 'Q'
pieceChar King = 'K'

gameMoveString :: GameMove -> String
gameMoveString move = concat [piece, orig, capture, dest, promotion, st, annotation]
    where
        piece = [pieceChar (gameMovePiece move)]
        orig = (encodeCoord . moveOrig . gameMoveMove) move
        capture = if gameMoveCapture move then "x" else ""
        dest = (encodeCoord . moveDest . gameMoveMove) move
        promotion = maybe "" (\p -> [pieceChar p]) (gameMovePromotion move)
        st = case gameMoveState move of
            Normal -> ""
            Check -> "+"
            Checkmate -> "#"
            Stalemate -> "="
        annotation = case gameMoveAnnotation move of
            Nothing -> ""
            Just Brilliant -> "!!"
            Just Good -> "!"
            Just Bad -> "?"
            Just Blunder -> "??"

gameMovesString :: St -> String
gameMovesString st =
    let moves = reverse $ getPastMoves (st ^. stGame)
        moveStrings = map gameMoveString moves
    -- TODO: we're currently counting moves wrong since one move is both white and black
    in unwords $ map (\(num :: Integer, move) -> show (num+1) ++ '.':move) (enumerate moveStrings)

drawMoves :: Int -> St -> Widget Name
drawMoves width st = hLimit width $ hCenter (str "Moves")
            <=> hBorder
            <=> strWrap (gameMovesString st)
