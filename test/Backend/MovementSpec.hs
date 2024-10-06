module Backend.MovementSpec (spec) where

import Test.Hspec

import Data.List (sort)

import Types
import Util
import Backend.Game
import Backend.Movement
import Types (Board)

emptyBoard :: Board
emptyBoard = Board $ map (const (emptyTiles 8)) [1 :: Integer .. 8]

checkmateBoard :: Board
checkmateBoard = Board [
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    Occ wr : emptyTiles 7,
    Occ wr : emptyTiles 6 ++ [Occ bk]]

---------------------------------
-- Piece Movement Tests
---------------------------------

data MovementTestCase = MovementTestCase
    { testBoard :: Board
    , testPos :: Position
    , expectedMoves :: [Position]
    }

checkMovementCase (MovementTestCase board pos expected) = 
    sort (validMovesAt board pos) `shouldBe` sort expected

setBoard' :: Piece -> Position -> Board -> Board
setBoard' piece pos board = setBoardAt board pos (Occ piece)

---------------------------------
-- Rook Movement
---------------------------------

-- Ensure the rook can move in straight lines when it's alone on the board.
rookBasicCase :: MovementTestCase
rookBasicCase = let
    testPosition = Vec2 4 4
    in MovementTestCase
    { testBoard = setBoardAt emptyBoard testPosition (Occ wr)
    , testPos = testPosition
    , expectedMoves = let 
            horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
            verticalMoves = map (\y -> Vec2 4 y) [0, 1, 2, 3, 5, 6, 7]
        in horizontalMoves ++ verticalMoves
    }

-- Ensure the rook can't move past friends and can capture enemies.
rookCollisionCase :: MovementTestCase
rookCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 (-2)   -- (4, 2)
    enemyPos = testPosition |+| Vec2 0 2         -- (4, 6)
    in MovementTestCase
    { testBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wr testPosition -- The rook we're examining
        ) emptyBoard
    , testPos = testPosition
    , expectedMoves = let
        horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
        verticalMoves = map (\y -> Vec2 4 y) [3, 5, 6]
        in horizontalMoves ++ verticalMoves
    }

rookMovementSpec :: Spec
rookMovementSpec = do
    describe "rook-movement" $ do
        it "can move in straight perpendicular lines" $ do
            checkMovementCase rookBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkMovementCase rookCollisionCase

---------------------------------
-- Bishop Movement
---------------------------------

-- Ensure the bishop can move in diagonal lines when it's alone on the board.
bishopBasicCase :: MovementTestCase
bishopBasicCase = let
    testPosition = Vec2 4 4
    in MovementTestCase
    { testBoard = setBoardAt emptyBoard testPosition (Occ wb)
    , testPos = testPosition
    , expectedMoves = let 
            diagonal1Moves = map (\x -> Vec2 x x) [0, 1, 2, 3, 5, 6, 7]
            diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in diagonal1Moves ++ diagonal2Moves
    }

-- Ensure the bishop can't move past friends and can capture enemies.
bishopCollisionCase :: MovementTestCase
bishopCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 (-2) (-2)   -- (2, 2)
    enemyPos = testPosition |+| Vec2 2 2            -- (6, 6)
    in MovementTestCase
    { testBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wb testPosition -- The rook we're examining
        ) emptyBoard
    , testPos = testPosition
    , expectedMoves = let
        diagonal1Moves = map (\x -> Vec2 x x) [3, 5, 6]
        diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in diagonal1Moves ++ diagonal2Moves
    }

bishopMovementSpec :: Spec
bishopMovementSpec = do
    describe "bishop-movement" $ do
        it "can move in diagonal lines" $ do
            checkMovementCase bishopBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkMovementCase bishopCollisionCase

movementSpec :: Spec
movementSpec = do
    describe "movement" $ do
        rookMovementSpec
        bishopMovementSpec

inCheckmateSpec :: Spec
inCheckmateSpec = do
    describe "inCheckmate" $ do
        it "returns True when the game is at checkmate" $ do
            inCheckmate Black checkmateBoard `shouldBe` True


spec :: Spec
spec = do
    describe "Movement" $ do
        inCheckmateSpec
        movementSpec