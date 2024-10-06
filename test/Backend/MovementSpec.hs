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

movementSpec :: Spec
movementSpec = do
    describe "movement" $ do
        rookMovementSpec
        bishopMovementSpec
        knightMovementSpec
        queenMovementSpec
        kingMovementSpec
        pawnMovementSpec

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
        . setBoard' wb testPosition -- The bishop we're examining
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

---------------------------------
-- Knight Movement
---------------------------------

knightTestBoardBase :: Board
knightTestBoardBase = Board [
    emptyTiles 8,
    emptyTiles 8,
    emptyTiles 8,
    -- Surround the Knight on 4,4 with pawns to ensure it jumps over pieces
    emptyTiles 3 ++ map Occ [wp, wp, wp] ++ emptyTiles 2,
    emptyTiles 3 ++ map Occ [wp, wn, wp] ++ emptyTiles 2,
    emptyTiles 3 ++ map Occ [wp, wp, wp] ++ emptyTiles 2,
    emptyTiles 8,
    emptyTiles 8]

-- Ensure the knight can move in L-patterns and jump over pieces
knightBasicCase :: MovementTestCase
knightBasicCase = let
    testPosition = Vec2 4 4
    in MovementTestCase
    { testBoard = knightTestBoardBase
    , testPos = testPosition
    , expectedMoves = map 
        (uncurry Vec2)
        [(2,5), (2,3), (3,2), (5,2), (6,3), (6,5), (5,6), (3,6)]
    }

-- Ensure the knight can't collide with friends but can capture enemies.
knightCollisionCase :: MovementTestCase
knightCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 1 2 -- (5, 6)
    enemyPos = testPosition |+| Vec2 (-1) 2 -- (3, 6)
    in MovementTestCase
    { testBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        ) knightTestBoardBase
    , testPos = testPosition
    , expectedMoves = map 
        (uncurry Vec2)
        [(2,5), (2,3), (3,2), (5,2), (6,3), (6,5), (3,6)]
    }

knightMovementSpec :: Spec
knightMovementSpec = do
    describe "knight-movement" $ do
        it "can move in L-patterns and over pieces" $ do
            checkMovementCase knightBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkMovementCase knightCollisionCase

---------------------------------
-- Queen Movement
---------------------------------

-- Ensure the queen can move in L-patterns and jump over pieces
queenBasicCase :: MovementTestCase
queenBasicCase = let
    testPosition = Vec2 4 4
    in MovementTestCase
    { testBoard = setBoardAt emptyBoard testPosition (Occ wq)
    , testPos = testPosition
    , expectedMoves = let
        horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
        verticalMoves = map (\y -> Vec2 4 y) [0, 1, 2, 3, 5, 6, 7]
        diagonal1Moves = map (\x -> Vec2 x x) [0, 1, 2, 3, 5, 6, 7]
        diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in horizontalMoves ++ verticalMoves ++ diagonal1Moves ++ diagonal2Moves
    }

-- Ensure the queen can't collide with friends but can capture enemies.
queenCollisionCase :: MovementTestCase
queenCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 2     -- (4, 6)
    enemyPos = testPosition |+| Vec2 (-2) (-2)  -- (2, 2)
    in MovementTestCase
    { testBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wq testPosition -- The queen we're examining
        ) emptyBoard
    , testPos = testPosition
    , expectedMoves = let
        horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
        verticalMoves = map (\y -> Vec2 4 y) [0, 1, 2, 3, 5]
        diagonal1Moves = map (\x -> Vec2 x x) [2, 3, 5, 6, 7]
        diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in horizontalMoves ++ verticalMoves ++ diagonal1Moves ++ diagonal2Moves
    }

queenMovementSpec :: Spec
queenMovementSpec = do
    describe "queen-movement" $ do
        it "can move in straight lines and diagonals" $ do
            checkMovementCase queenBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkMovementCase queenCollisionCase

---------------------------------
-- King Movement
---------------------------------

kingBasicCase :: MovementTestCase
kingBasicCase = let
    testPosition = Vec2 4 4
    in MovementTestCase
    { testBoard = setBoardAt emptyBoard testPosition (Occ wk)
    , testPos = testPosition
    , expectedMoves = map (uncurry Vec2) [(3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)]
    }

kingCollisionCase :: MovementTestCase
kingCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 1 -- (4, 5)
    enemyPos = testPosition |+| Vec2 (-1) 1 -- (5, 3)
    in MovementTestCase
    { testBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wk testPosition -- The king we're examining
        ) emptyBoard
    , testPos = testPosition
    , expectedMoves = map (uncurry Vec2) [(3,3), (3,4), (3,5), (4,3), (5,3), (5,4), (5,5)]
    }

kingMovementSpec :: Spec
kingMovementSpec = do
    describe "king-movement" $ do
        it "can move to adjacent squares in all directions" $ do
            checkMovementCase kingBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkMovementCase kingCollisionCase

---------------------------------
-- Pawn Movement
---------------------------------

pawnTestBoard :: Board
pawnTestBoard = Board [
    emptyTiles 8,
    -- Test 1a: black pawn on starting position to test double move (0,1)
    Occ bp : emptyTiles 7, 
    -- Test 2a: black pawn on starting position to test normal movement (1,2)
    -- Test 3a: black pawn blocked from moving forward by another piece (3,2)
    -- Test 4a: black pawn with an enemy pieces it can capture (6,2)
    [Empty, Occ bp, Empty, Occ bp, Empty, Empty, Occ bp, Empty], 
    emptyTiles 3 ++ [Occ wp, Empty, Occ wp, Empty, Occ wp],
    -- Test 4b: white pawn with an enemy pieces it can capture (6,5)
    -- Test 3b: white pawn blocked from moving forward by another piece (3,5)
    -- Test 2b: white pawn on starting position to test normal movement (1,5)
    emptyTiles 3 ++ [Occ bp, Empty, Occ bp, Empty, Occ bp],
    [Empty, Occ wp, Empty, Occ wp, Empty, Empty, Occ wp, Empty], 
    -- Test 1b: white pawn on starting position to test double move (0,6)
    Occ wp : emptyTiles 7, 
    emptyTiles 8]

-- Test case 1: Starting movement
----------------------------------
blackPawnStarting :: MovementTestCase
blackPawnStarting = let
    testPosition = Vec2 0 1
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [testPosition |+| downDirection, testPosition |+| (2 |*| downDirection)]
    }

whitePawnStarting :: MovementTestCase
whitePawnStarting = let
    testPosition = Vec2 0 6
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [testPosition |+| upDirection, testPosition |+| (2 |*| upDirection)]
    }

-- Test case 2: Normal movement
----------------------------------
blackPawnNormal :: MovementTestCase
blackPawnNormal = let
    testPosition = Vec2 1 2
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [testPosition |+| downDirection]
    }

whitePawnNormal :: MovementTestCase
whitePawnNormal = let
    testPosition = Vec2 1 5
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [testPosition |+| upDirection]
    }

-- Test case 3: Blocked movement
----------------------------------
blackPawnBlocked :: MovementTestCase
blackPawnBlocked = let
    testPosition = Vec2 3 2
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = []
    }

whitePawnBlocked :: MovementTestCase
whitePawnBlocked = let
    testPosition = Vec2 3 5
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = []
    }

-- Test case 4: Capture
----------------------------------
blackPawnCapture :: MovementTestCase
blackPawnCapture = let
    testPosition = Vec2 6 2
    forward = testPosition |+| downDirection
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [forward, forward |+| leftDirection, forward |+| rightDirection]
    }

whitePawnCapture :: MovementTestCase
whitePawnCapture = let
    testPosition = Vec2 6 5
    forward = testPosition |+| upDirection
    in MovementTestCase
    { testBoard = pawnTestBoard
    , testPos = testPosition
    , expectedMoves = [forward, forward |+| leftDirection, forward |+| rightDirection]
    }

pawnMovementSpec :: Spec
pawnMovementSpec = do
    describe "pawn-movement" $ do
        describe "starting-moves" $ do
            it "black pawns can move two spaces down" $ do
                checkMovementCase blackPawnStarting
            it "white pawns can move two spaces up" $ do
                checkMovementCase whitePawnStarting
        describe "normal-moves" $ do
            it "black pawns can move one space down" $ do
                checkMovementCase blackPawnNormal
            it "white pawns can move one space up" $ do
                checkMovementCase whitePawnNormal
        describe "blocked-moves" $ do
            it "black pawns can't move with a piece in front of them" $ do
                checkMovementCase blackPawnBlocked
            it "white pawns can't move with a piece in front of them" $ do
                checkMovementCase whitePawnBlocked
        describe "capture-moves" $ do
            it "black pawns can capture on adjacent diagonal squares" $ do
                checkMovementCase blackPawnCapture
            it "white pawns can capture on adjacent diagonal squares" $ do
                checkMovementCase whitePawnCapture

---------------------------------
-- Check and Mate Tests
---------------------------------

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