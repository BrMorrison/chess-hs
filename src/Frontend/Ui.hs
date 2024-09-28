{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- This module contains the TUI code for running the chess program
module Frontend.Ui (uiMain) where

import Brick
import Brick.Widgets.Center
import qualified Brick.Widgets.Edit as E

import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Control.Monad.Trans.State (runState)

import Types
import Frontend.Client (evalCmd, drawGame)
import Backend.Interface (newGame)

newtype Name = Name ()
    deriving(Eq, Ord, Show)

data St = St { _stGame :: Game
             , _stMsg :: String
             , _stInput :: E.Editor String Name
             }
makeLenses ''St

draw :: St -> [Widget Name]
draw st = [ui]
    where
        e = E.renderEditor (str . unlines) True (_stInput st)
        ui = center $
            hCenter ((str . drawGame) (st ^. stGame)) <=>
            hCenter (str " " <=>
            hLimit 75 (str (st ^. stMsg)) <=>
            (str ">" <+> hLimit 74 e))

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