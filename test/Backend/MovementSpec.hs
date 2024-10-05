module Backend.MovementSpec (spec) where

import Test.Hspec

import Types
import Backend.Game
import Backend.Movement

checkmateBoard :: Board
checkmateBoard = Board [
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    map (const Empty) [1 ::Integer .. 8],
    (Occ wr) : map (const Empty) [ 1 :: Integer .. 7],
    (Occ wr) : map (const Empty) [ 1 :: Integer .. 6] ++ [Occ bk]]

spec :: Spec
spec = do
    describe "inCheckmate" $ do
        it "returns True when the game is at checkmate" $ do
            (inCheckmate Black checkmateBoard) `shouldBe` True