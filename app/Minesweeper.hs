{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom
import System.Random
import Data.Set as Set (Set,empty,insert,delete,size,member,notMember,(\\),intersection,union,toList,fromList)
import qualified Data.Set as Set
import Data.List (nub, find, intersperse)
import Data.Map.Strict as Map (Map, fromList)
import Data.Text  (Text, pack, unpack)
import Text.Printf
import Control.Monad(mapM, foldM)
import Data.Tuple (swap)

-- Types and defaults

-- A Minesweeper game is defined by a board dimension, and the sets of
-- mined, flagged and revealed coordinates.
data Game = Game { mines :: Coords,
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

startDifficulty = ((8,8), 10)


-- GUI

main = do
  seed <- randomIO
  let g = mkStdGen seed
  mainWidget $ do
     el "h1" $ text "Reflex FRP Minesweeper"
     Dropdown diffD diffE <- difficultyMenu
     newGameBtnE <- button "New game"
     el "br" blank
     revealBtnE <- button "Reveal"
     flagBtnE <- button "Flag"
     currentActionB <- hold click $ leftmost [ (click <$ revealBtnE)
                                              , (toggleFlag <$ flagBtnE)]
     currentSeedDyn <- foldDyn (\_ (i,g) -> next g) (next g) newGameBtnE
     let newGameE = tag (current diffD) newGameBtnE
     let newGameSeedE = attachPromptlyDyn (fst <$> currentSeedDyn) newGameE
     el "br" blank
     widgetHold (aGame currentActionB (seed, startDifficulty)) (aGame currentActionB <$> newGameSeedE)
     return ()

board :: MonadWidget t m => Game -> m (Event t Coordinate)
board g =
  let fields = Set.toList $ allFields (dim g)
  in do
    let (w,h) = dim g
    e <- elAttr "div" (Map.fromList
                       [("style", pack $ printf
                          "position:relative;width:%dpx;height:%dpx;border:3px solid AliceBlue" (w*40) (h*40))])
         $ (leftmost <$>
            mapM (\(x,y) -> do
                     let cellStatus = statusFor (x,y) g
                     e <- cell (status g) cellStatus (x,y) 40
                     return $ (x,y) <$ e)
            fields)
    return e

css :: [(String,String)] -> Text
css = pack . foldl (++) "" . Data.List.intersperse ";" . map (\ (a,v) -> a ++ ":" ++ v)

cell :: MonadWidget t m => BoardStatus -> CellStatus -> Coordinate -> Int -> m (Event t ())
cell stat (numNeighbours, mined, revealed, flagged) (x,y) wh = do
  (btn,_) <- elAttr' "button"
    (Map.fromList $
     [ ("style", css [ ("position", "absolute")
                     , ("top", show (x * wh + 1) ++ "px")
                     , ("left", show (y * wh + 1) ++ "px")
                     , ("width", show (wh - 2) ++ "px")
                     , ("height", show (wh - 2) ++ "px")
                     , ("border", "none")
                     , ("background-color",
                        case stat of
                          Lost -> case' "white"
                                     [ (mined && flagged, "green")
                                     , (mined && not flagged, "red")
                                     , (flagged, "red") ]
                          Won -> case' "white"
                                     [ (mined, "green") ]
                          Alive -> if revealed then "white" else "grey")]) ])
    (text . pack $
      case stat of
        Alive -> case' "" [ (flagged, "⚑")
                          , (revealed && numNeighbours > 0, show numNeighbours)]
        _ -> case' ""
                   [ (flagged, "⚑")
                   , (mined, "💣")
                   , (numNeighbours > 0, show numNeighbours)])
  return $ domEvent Click btn

difficultyMenu :: MonadWidget t m => m (Dropdown t (Coordinate, Int))
difficultyMenu = dropdown startDifficulty (constDyn difficulties) def

game :: MonadWidget t m => Behavior t Action -> Game -> m ()
game actionB initGame = do
  rec -- :: Dynamic Game
    gDyn <- foldDyn ($) initGame $ (flip <$> actionB) <@> clickE
    -- :: Event Coordinate
    clickE <- (dyn $ board <$> gDyn) >>= switchPromptly never
  return ()

andThen :: MonadWidget t m => m (Event t a) -> (a -> m b) -> m ()
andThen start next = do
  -- andThenDyn :: Dynamic (Event t a)
  rec andThenDyn <- widgetHold start nextE
      -- nextE :: Event t (m (Event t a))
      let nextE = (never <$) <$> next <$> switchPromptlyDyn andThenDyn
  return ()

aGame :: MonadWidget t m => Behavior t Action -> (Int, Difficulty) -> m ()
aGame actionB (seed, difficulty@(size, _)) =
  andThen (board (emptyGame size)) (game actionB . makeGame difficulty seed)


-- Game mechanics

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
emptyGame dim = Game empty empty empty dim

makeGame :: Difficulty -> Int -> Coordinate -> Game
makeGame (dim,numMines) seed init =
    click (makeGame' (dim,numMines) seed init)
          init
makeGame' (dim,numMines) seed init =
    Game (makeMines init dim seed numMines) empty empty dim

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

status :: Game -> BoardStatus
status g | mines g `intersection` reveals g /= empty      = Lost
         | mines g `union` reveals g == allFields (dim g) = Won
         | otherwise                                      = Alive

numNeighbours :: Game -> Coordinate -> Int
numNeighbours g c = size $ neighbours c (dim g) `intersection` mines g

allFields :: BoardDim -> Coords
allFields (dx,dy) = Set.fromList [(x,y) | x <- [0..dx-1], y <- [0..dy-1]]

neighbours :: Coordinate -> BoardDim -> Coords
neighbours (x, y) (bx, by) =
  Set.fromList . filter (\(x,y) -> (x >= 0 && x < bx && y >= 0 && y < by)) $
  [ (x+1,y),(x,y+1),(x-1,y),(x,y-1)
  , (x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

statusFor :: Coordinate -> Game -> CellStatus
statusFor c g =
  (numNeighbours g c, member c (mines g), member c (reveals g), member c (flags g))


-- Utilities

-- from https://wiki.haskell.org/Case
case' :: a -> [(Bool, a)] -> a
case' def = maybe def snd . find fst

