{-# LANGUAGE OverloadedStrings #-}

module Minesweeper where

import System.Random
import Data.Set as Set (Set,empty,insert,delete,size,member,notMember,(\\),intersection,union,toList,fromList)
import qualified Data.Set as Set
import Data.List (nub, find, intersperse)
import Data.Map.Strict as Map (Map, fromList)
import Data.Text  (Text, pack, unpack)
import Data.Tuple (swap)

-- Types and defaults:

-- A Minesweeper game is defined by a board dimension, and the sets of
-- mined, flagged and revealed coordinates.
data Game = Game { fields :: Coords,
                   mines :: Coords,
                   reveals :: Coords,
                   flags :: Coords,
                   dim :: BoardDim } deriving Show

type Coords = Set Coordinate
type Coordinate = (Int, Int)
type BoardDim = (Int, Int)
data BoardStatus = Alive | Lost | Won deriving Show
type Action = Game -> Coordinate -> Game
type Difficulty = (BoardDim, Int)
type CellStatus = (Int, Bool, Bool, Bool)

difficulties :: Map (BoardDim, Int) Text
difficulties = Map.fromList . map Data.Tuple.swap $
  [("Beginner", ((8,8), 10)),
    ("Intermediate", ((16,16), 40)),
    ("Expert", ((30,16), 99))]

initDifficulty = ((8,8), 10) :: Difficulty


-- Game mechanics:

-- A set of n coordinates for mines so that the initial click has no
-- neighbouring mines. WARNING: will loop forever if there is no
-- valid mine placement following this rule.
makeMines :: Coordinate -> BoardDim -> Int -> Int -> Coords
makeMines init dim@(dx, dy) seed n =
    let (rx, ry)     = split (mkStdGen seed)
        randomCoords = zip (randomRs (0,dx-1) rx) (randomRs (0,dy-1) ry)
    in Set.fromList $ take n $ nub $
         filter (`notMember` (insert init $ neighbours init dim))
                randomCoords

emptyGame :: BoardDim -> Game
emptyGame (dx,dy) =
  Game (Set.fromList [(x,y) | x <- [0..dx-1], y <- [0..dy-1]])
       empty empty empty (dx,dy)

makeGame :: Difficulty -> Int -> Coordinate -> Game
makeGame (dim,numMines) seed init =
  click ((emptyGame dim) {mines = makeMines init dim seed numMines}) init

click :: Game -> Coordinate -> Game
click g c
    -- A click on a cell that's already revealed reveals the
    -- un-flagged cells around that cell.
    | c `member` reveals g = revealAround g c
    -- If a cell has neighbouring mines simply reveal the cell.
    | numNeighbours g c > 0 = reveal g c
    -- Reveal all neighbouring cells if a cell has no neighbouring
    -- mines.
    | otherwise = Set.foldl click (reveal g c)
                  $ (neighbours c (dim g) \\ (reveals g))

flag, unflag, toggleFlag, revealAround, reveal :: Action

flag g c = g {flags =
              if c `member` reveals g
              then flags g
              else insert c $ flags g}

unflag g c = g {flags = delete c $ flags g}

toggleFlag g c = (if c `member` flags g then unflag else flag) g c

-- RevealAround safely clears the unflagged fields around a revealed field
revealAround g c
  | (size $ neighbours c (dim g) `intersection` flags g) >= numNeighbours g c =
      Set.foldl click g $ (neighbours c (dim g) \\ flags g \\ reveals g)
  | otherwise = g

reveal g c = g {reveals = c `insert` reveals g}


status :: Game -> BoardStatus
status g | mines g `intersection` reveals g /= empty      = Lost
         | mines g `union` reveals g == fields g          = Won
         | otherwise                                      = Alive

numNeighbours :: Game -> Coordinate -> Int
numNeighbours g c = size $ neighbours c (dim g) `intersection` mines g

neighbours :: Coordinate -> BoardDim -> Coords
neighbours (x, y) (dx,dy) =
  Set.fromList . filter (\(x,y) -> (x >= 0 && x < dx && y >= 0 && y < dy)) $
               [ (x+1,y),(x,y+1),(x-1,y),(x,y-1)
               , (x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

statusFor :: Coordinate -> Game -> CellStatus
statusFor c g =
  (numNeighbours g c, member c (mines g), member c (reveals g), member c (flags g))
