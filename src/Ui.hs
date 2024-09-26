{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Ui (uiMain) where

import Brick
import Brick.Widgets.Center
import qualified Brick.Widgets.Edit as E

import qualified Graphics.Vty as V
import Lens.Micro.Platform
-- import Lens.Micro
-- import Lens.Micro.TH (makeLenses)
-- import Lens.Micro.Mtl

import Game
import Repl (eval')
import Control.Monad.Trans.State (runState)

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
        e = E.renderEditor (str . unlines) False (_stInput st)
        ui = center $
            str (show (st ^. stGame)) <=>
            str " " <=>
            str (st ^. stMsg) <=>
            (str ">" <+> hLimit 30 e)

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    -- I'm sure there's a better way, but for now, this should do.
    let cmd = head $ E.getEditContents (st ^. stInput)
        (msg, game') = runState (eval' (words cmd)) (st ^. stGame)
        in do
            stMsg .= msg
            stGame .= game'
            -- Send the commands to the editor that should clear it
            zoom stInput $ E.handleEditorEvent (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl]))
            zoom stInput $ E.handleEditorEvent (VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl]))
            return ()
handleEvent ev = zoom stInput $ E.handleEditorEvent ev

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (mempty, V.white `on` V.black) ]

initState :: St
initState = St { _stGame = initGame
               , _stMsg = ""
               , _stInput = E.editor (Name ()) (Just 1) ""
               }

app :: App St e Name
app = App { appDraw = draw
          , appHandleEvent = handleEvent
          , appChooseCursor = \_ _ -> Nothing
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

uiMain :: IO ()
uiMain = do
    _ <- defaultMain app initState
    return ()