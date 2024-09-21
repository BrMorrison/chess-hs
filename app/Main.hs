module Main where

import Game
import Repl

main :: IO ()
main = do
    putStrLn $ show initGame ++ "\nType \"help\" for commands."
    repl initGame