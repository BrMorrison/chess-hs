module Backend.MovementSpec (spec) where

import Test.Hspec

import Data.List (sort)

import Types
import Util
import Backend.Game
import Backend.Movement
import Types (Board)

spec :: Spec
spec = do
    describe "Movement" $ do
        checkGameStateSpec
        validMovesAtSpec
        makeMoveSpec

emptyBoard :: Board
emptyBoard = Board $ map (const (emptyTiles 8)) [1 :: Integer .. 8]

setBoard' :: Piece -> Position -> Board -> Board
setBoard' piece pos board = setBoardAt board pos (Occ piece)

------------------------------------------------------------------
--                  validMovesAt Tests
------------------------------------------------------------------

data ValidMovesAtTestCase = ValidMovesAtTestCase
    { vmaTestBoard :: Board
    , vmaTestPos:: Position
    , vmaExpectedMoves :: [Position]
    }

checkValidMoveAtCase :: ValidMovesAtTestCase -> Expectation
checkValidMoveAtCase (ValidMovesAtTestCase board pos expected) = 
    sort (validMovesAt board pos) `shouldBe` sort expected

validMovesAtSpec :: Spec
validMovesAtSpec = do
    describe "validMovesAt" $ do
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
rookBasicCase :: ValidMovesAtTestCase
rookBasicCase = let
    testPosition = Vec2 4 4
    in ValidMovesAtTestCase
    { vmaTestBoard = setBoardAt emptyBoard testPosition (Occ wr)
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let 
            horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
            verticalMoves = map (\y -> Vec2 4 y) [0, 1, 2, 3, 5, 6, 7]
        in horizontalMoves ++ verticalMoves
    }

-- Ensure the rook can't move past friends and can capture enemies.
rookCollisionCase :: ValidMovesAtTestCase
rookCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 (-2)   -- (4, 2)
    enemyPos = testPosition |+| Vec2 0 2         -- (4, 6)
    in ValidMovesAtTestCase
    { vmaTestBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wr testPosition -- The rook we're examining
        ) emptyBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let
        horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
        verticalMoves = map (\y -> Vec2 4 y) [3, 5, 6]
        in horizontalMoves ++ verticalMoves
    }

rookMovementSpec :: Spec
rookMovementSpec = do
    describe "rook-movement" $ do
        it "can move in straight perpendicular lines" $ do
            checkValidMoveAtCase rookBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkValidMoveAtCase rookCollisionCase

---------------------------------
-- Bishop Movement
---------------------------------

-- Ensure the bishop can move in diagonal lines when it's alone on the board.
bishopBasicCase :: ValidMovesAtTestCase
bishopBasicCase = let
    testPosition = Vec2 4 4
    in ValidMovesAtTestCase
    { vmaTestBoard = setBoardAt emptyBoard testPosition (Occ wb)
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let 
            diagonal1Moves = map (\x -> Vec2 x x) [0, 1, 2, 3, 5, 6, 7]
            diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in diagonal1Moves ++ diagonal2Moves
    }

-- Ensure the bishop can't move past friends and can capture enemies.
bishopCollisionCase :: ValidMovesAtTestCase
bishopCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 (-2) (-2)   -- (2, 2)
    enemyPos = testPosition |+| Vec2 2 2            -- (6, 6)
    in ValidMovesAtTestCase
    { vmaTestBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wb testPosition -- The bishop we're examining
        ) emptyBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let
        diagonal1Moves = map (\x -> Vec2 x x) [3, 5, 6]
        diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in diagonal1Moves ++ diagonal2Moves
    }

bishopMovementSpec :: Spec
bishopMovementSpec = do
    describe "bishop-movement" $ do
        it "can move in diagonal lines" $ do
            checkValidMoveAtCase bishopBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkValidMoveAtCase bishopCollisionCase

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
knightBasicCase :: ValidMovesAtTestCase
knightBasicCase = let
    testPosition = Vec2 4 4
    in ValidMovesAtTestCase
    { vmaTestBoard = knightTestBoardBase
    , vmaTestPos= testPosition
    , vmaExpectedMoves = map 
        (uncurry Vec2)
        [(2,5), (2,3), (3,2), (5,2), (6,3), (6,5), (5,6), (3,6)]
    }

-- Ensure the knight can't collide with friends but can capture enemies.
knightCollisionCase :: ValidMovesAtTestCase
knightCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 1 2 -- (5, 6)
    enemyPos = testPosition |+| Vec2 (-1) 2 -- (3, 6)
    in ValidMovesAtTestCase
    { vmaTestBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        ) knightTestBoardBase
    , vmaTestPos= testPosition
    , vmaExpectedMoves = map 
        (uncurry Vec2)
        [(2,5), (2,3), (3,2), (5,2), (6,3), (6,5), (3,6)]
    }

knightMovementSpec :: Spec
knightMovementSpec = do
    describe "knight-movement" $ do
        it "can move in L-patterns and over pieces" $ do
            checkValidMoveAtCase knightBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkValidMoveAtCase knightCollisionCase

---------------------------------
-- Queen Movement
---------------------------------

-- Ensure the queen can move in L-patterns and jump over pieces
queenBasicCase :: ValidMovesAtTestCase
queenBasicCase = let
    testPosition = Vec2 4 4
    in ValidMovesAtTestCase
    { vmaTestBoard = setBoardAt emptyBoard testPosition (Occ wq)
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let
        horizontalMoves = map (\x -> Vec2 x 4) [0, 1, 2, 3, 5, 6, 7]
        verticalMoves = map (\y -> Vec2 4 y) [0, 1, 2, 3, 5, 6, 7]
        diagonal1Moves = map (\x -> Vec2 x x) [0, 1, 2, 3, 5, 6, 7]
        diagonal2Moves = map (\x -> Vec2 (x+1) (7-x)) [0, 1, 2, 4, 5, 6]
        in horizontalMoves ++ verticalMoves ++ diagonal1Moves ++ diagonal2Moves
    }

-- Ensure the queen can't collide with friends but can capture enemies.
queenCollisionCase :: ValidMovesAtTestCase
queenCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 2     -- (4, 6)
    enemyPos = testPosition |+| Vec2 (-2) (-2)  -- (2, 2)
    in ValidMovesAtTestCase
    { vmaTestBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wq testPosition -- The queen we're examining
        ) emptyBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = let
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
            checkValidMoveAtCase queenBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkValidMoveAtCase queenCollisionCase

---------------------------------
-- King Movement
---------------------------------

kingBasicCase :: ValidMovesAtTestCase
kingBasicCase = let
    testPosition = Vec2 4 4
    in ValidMovesAtTestCase
    { vmaTestBoard = setBoardAt emptyBoard testPosition (Occ wk)
    , vmaTestPos= testPosition
    , vmaExpectedMoves = map (uncurry Vec2) [(3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)]
    }

kingCollisionCase :: ValidMovesAtTestCase
kingCollisionCase = let
    testPosition = Vec2 4 4
    friendlyPos = testPosition |+| Vec2 0 1 -- (4, 5)
    enemyPos = testPosition |+| Vec2 (-1) 1 -- (5, 3)
    in ValidMovesAtTestCase
    { vmaTestBoard =
        ( setBoard' bp enemyPos     -- Add an enemy two spaces up
        . setBoard' wp friendlyPos  -- Add a friend one 
        . setBoard' wk testPosition -- The king we're examining
        ) emptyBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = map (uncurry Vec2) [(3,3), (3,4), (3,5), (4,3), (5,3), (5,4), (5,5)]
    }

kingMovementSpec :: Spec
kingMovementSpec = do
    describe "king-movement" $ do
        it "can move to adjacent squares in all directions" $ do
            checkValidMoveAtCase kingBasicCase
        it "collides with friendly and enemy pieces" $ do
            checkValidMoveAtCase kingCollisionCase

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
blackPawnStarting :: ValidMovesAtTestCase
blackPawnStarting = let
    testPosition = Vec2 0 1
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [testPosition |+| downDirection, testPosition |+| (2 |*| downDirection)]
    }

whitePawnStarting :: ValidMovesAtTestCase
whitePawnStarting = let
    testPosition = Vec2 0 6
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [testPosition |+| upDirection, testPosition |+| (2 |*| upDirection)]
    }

-- Test case 2: Normal movement
----------------------------------
blackPawnNormal :: ValidMovesAtTestCase
blackPawnNormal = let
    testPosition = Vec2 1 2
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [testPosition |+| downDirection]
    }

whitePawnNormal :: ValidMovesAtTestCase
whitePawnNormal = let
    testPosition = Vec2 1 5
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [testPosition |+| upDirection]
    }

-- Test case 3: Blocked movement
----------------------------------
blackPawnBlocked :: ValidMovesAtTestCase
blackPawnBlocked = let
    testPosition = Vec2 3 2
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = []
    }

whitePawnBlocked :: ValidMovesAtTestCase
whitePawnBlocked = let
    testPosition = Vec2 3 5
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = []
    }

-- Test case 4: Capture
----------------------------------
blackPawnCapture :: ValidMovesAtTestCase
blackPawnCapture = let
    testPosition = Vec2 6 2
    forward = testPosition |+| downDirection
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [forward, forward |+| leftDirection, forward |+| rightDirection]
    }

whitePawnCapture :: ValidMovesAtTestCase
whitePawnCapture = let
    testPosition = Vec2 6 5
    forward = testPosition |+| upDirection
    in ValidMovesAtTestCase
    { vmaTestBoard = pawnTestBoard
    , vmaTestPos= testPosition
    , vmaExpectedMoves = [forward, forward |+| leftDirection, forward |+| rightDirection]
    }

pawnMovementSpec :: Spec
pawnMovementSpec = do
    describe "pawn-movement" $ do
        describe "starting-moves" $ do
            it "black pawns can move two spaces down" $ do
                checkValidMoveAtCase blackPawnStarting
            it "white pawns can move two spaces up" $ do
                checkValidMoveAtCase whitePawnStarting
        describe "normal-moves" $ do
            it "black pawns can move one space down" $ do
                checkValidMoveAtCase blackPawnNormal
            it "white pawns can move one space up" $ do
                checkValidMoveAtCase whitePawnNormal
        describe "blocked-moves" $ do
            it "black pawns can't move with a piece in front of them" $ do
                checkValidMoveAtCase blackPawnBlocked
            it "white pawns can't move with a piece in front of them" $ do
                checkValidMoveAtCase whitePawnBlocked
        describe "capture-moves" $ do
            it "black pawns can capture on adjacent diagonal squares" $ do
                checkValidMoveAtCase blackPawnCapture
            it "white pawns can capture on adjacent diagonal squares" $ do
                checkValidMoveAtCase whitePawnCapture

------------------------------------------------------------------
--                  checkGameState Tests
------------------------------------------------------------------

data CheckGameStateTestCase = CheckGameStateTestCase
    { cgsTestBoard :: Board
    , cgsTestColor :: Color
    , cgsExpectedState :: GameState
    }

checkCheckGameStateCase :: CheckGameStateTestCase -> Expectation
checkCheckGameStateCase (CheckGameStateTestCase board color expected) = 
    checkGameState color board `shouldBe` expected

checkmateTestCase :: CheckGameStateTestCase
checkmateTestCase = CheckGameStateTestCase
    { cgsTestBoard = 
        ( setBoard' wr (Vec2 0 7)
        . setBoard' wr (Vec2 0 6)
        . setBoard' bk (Vec2 7 7)
        ) emptyBoard
    , cgsTestColor = Black
    , cgsExpectedState = Checkmate
    }

checkTestCase :: CheckGameStateTestCase
checkTestCase = CheckGameStateTestCase
    { cgsTestBoard =
        ( setBoard' wr (Vec2 0 7)
        . setBoard' bk (Vec2 7 7)
        ) emptyBoard
    , cgsTestColor = Black
    , cgsExpectedState = Check
    }

stalemateTestCase :: CheckGameStateTestCase
stalemateTestCase = CheckGameStateTestCase
    { cgsTestBoard =
        ( setBoard' wr (Vec2 6 0)
        . setBoard' wr (Vec2 0 6)
        . setBoard' bk (Vec2 7 7)
        ) emptyBoard
    , cgsTestColor = Black
    , cgsExpectedState = Stalemate
    }

noCheckTestCase :: CheckGameStateTestCase
noCheckTestCase = CheckGameStateTestCase
    { cgsTestBoard =
        ( setBoard' wr (Vec2 6 0)
        . setBoard' bk (Vec2 7 7)
        ) emptyBoard
    , cgsTestColor = Black
    , cgsExpectedState = Normal
    }

checkGameStateSpec :: Spec
checkGameStateSpec = do
    describe "checkGameState" $ do
        it "correctly identifies a checkmate" $ do
            checkCheckGameStateCase checkmateTestCase
        it "correctly identifies a check" $ do
            checkCheckGameStateCase checkTestCase
        it "correctly identifies a stalemate" $ do
            checkCheckGameStateCase stalemateTestCase
        it "correctly identifies a normal game state" $ do
            checkCheckGameStateCase noCheckTestCase

------------------------------------------------------------------
--                  makeMove Tests
------------------------------------------------------------------

data MakeMoveTestCase = MakeMoveTestCase
    { mmTestGame :: Game
    , mmTestMove :: Move
    , mmExpected :: Maybe Game
    }

checkMakeMoveCase :: MakeMoveTestCase -> Expectation
checkMakeMoveCase (MakeMoveTestCase game move expected) = 
    makeMove move game `shouldBe` expected

makeMoveSpec :: Spec
makeMoveSpec = do
    describe "makeMove" $ do
        it "white pawn can move and capture to put king in check" $ do
            checkMakeMoveCase whitePawnCaptureToPromoteAndCheckTestCase
        it "black pawn can move and promote without capture or check" $ do
            checkMakeMoveCase blackPawnPromoteTestCase
        it "white king must move out of check" $ do
            checkMakeMoveCase whiteKingFailsToMoveIntoCheckTestCase
        it "can't move on the wrong turn" $ do
            checkMakeMoveCase whitePawnFailsToMoveOnBlackTurnTestCase


makeMoveTestBoard :: Board
makeMoveTestBoard = Board 
    [ [Empty, Occ br, Occ bk] ++ emptyTiles 5
    -- Test 1: White pawn promotes captures to promote and check the king PA7xB8Q+
    -- Test 2: Ensure you're required to move out of check. e.g., PA7A8Q is not valid.
    , Occ wp : emptyTiles 7
    , emptyTiles 8
    , emptyTiles 8
    , [Empty, Empty, Occ wk] ++ emptyTiles 5
    , emptyTiles 8
    -- Test 3: black pawn on promotes PA2A1Q
    , Occ bp : emptyTiles 7
    , emptyTiles 8
    ]

----------------------
-- Test Cases
----------------------

whitePawnCaptureToPromoteAndCheckTestCase :: MakeMoveTestCase
whitePawnCaptureToPromoteAndCheckTestCase = let
    testMove = Move (Vec2 0 1) (Vec2 1 0)
    expectedGameMove = GameMove
        { gameMovePiece      = Pawn
        , gameMoveCapture    = True
        , gameMoveMove       = testMove
        , gameMovePromotion  = Just Queen
        , gameMoveState      = Check
        , gameMoveAnnotation = Nothing
        }
    in MakeMoveTestCase
        { mmTestGame = Game 
            { gameBoard = makeMoveTestBoard
            , gameTurn  = White
            , gameMoves = []
            , gameState = Normal
            }
        , mmTestMove = testMove
        , mmExpected = Just $ Game
            { gameBoard = movePiece wq testMove makeMoveTestBoard
            , gameTurn  = Black
            , gameMoves = [expectedGameMove]
            , gameState = Check
            }
        }

blackPawnPromoteTestCase :: MakeMoveTestCase
blackPawnPromoteTestCase = let
    testMove = Move (Vec2 0 6) (Vec2 0 7)
    expectedGameMove = GameMove
        { gameMovePiece      = Pawn
        , gameMoveCapture    = False
        , gameMoveMove       = testMove
        , gameMovePromotion  = Just Queen
        , gameMoveState      = Normal
        , gameMoveAnnotation = Nothing
        }
    in MakeMoveTestCase
        { mmTestGame = Game 
            { gameBoard = makeMoveTestBoard
            , gameTurn  = Black
            , gameMoves = []
            , gameState = Normal
            }
        , mmTestMove = testMove
        , mmExpected = Just $ Game
            { gameBoard = movePiece bq testMove makeMoveTestBoard
            , gameTurn  = White
            , gameMoves = [expectedGameMove]
            , gameState = Normal
            }
        }

whiteKingFailsToMoveIntoCheckTestCase :: MakeMoveTestCase
whiteKingFailsToMoveIntoCheckTestCase = let
    testMove = Move (Vec2 1 4) (Vec2 1 5)
    -- Have the king start in check
    testBoard = movePiece wk (Move (Vec2 2 4) (Vec2 1 4)) makeMoveTestBoard
    in MakeMoveTestCase
        { mmTestGame = Game 
            { gameBoard = testBoard
            , gameTurn  = White
            , gameMoves = []
            , gameState = Check
            }
        , mmTestMove = testMove
        , mmExpected = Nothing
        }

whitePawnFailsToMoveOnBlackTurnTestCase :: MakeMoveTestCase
whitePawnFailsToMoveOnBlackTurnTestCase = let
    testMove = Move (Vec2 0 1) (Vec2 1 0)
    in MakeMoveTestCase
        { mmTestGame = Game 
            { gameBoard = makeMoveTestBoard
            , gameTurn  = Black
            , gameMoves = []
            , gameState = Normal
            }
        , mmTestMove = testMove
        , mmExpected = Nothing
        }