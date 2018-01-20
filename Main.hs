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
import Minesweeper

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
     widgetHold (aGame currentActionB (seed, initDifficulty)) (aGame currentActionB <$> newGameSeedE)
     return ()

board :: MonadWidget t m => Game -> m (Event t Coordinate)
board g =
  do
    let (w,h) = dim g
    e <- elAttr "div" (Map.fromList
                       [("style", pack $ printf
                          "position:relative;width:%dpx;height:%dpx;border:3px solid AliceBlue" (w*40) (h*40))])
         $ (leftmost <$>
            mapM (\(x,y) -> do
                     let cellStatus = statusFor (x,y) g
                     e <- cell (status g) cellStatus (x,y) 40
                     return $ (x,y) <$ e)
            (Set.toList $ fields g))
    return e

cell :: MonadWidget t m => BoardStatus -> CellStatus -> Coordinate -> Int -> m (Event t ())
cell stat (numNeighbours, mined, revealed, flagged) (x,y) wh = do
  (btn,_) <- elAttr' "button"
    (Map.fromList $
     [ ("style", css [ ("position", "absolute")
                     , ("left", show (x * wh + 1) ++ "px")
                     , ("top", show (y * wh + 1) ++ "px")
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
difficultyMenu =
  dropdown initDifficulty (constDyn difficulties) def

game :: MonadWidget t m => Behavior t Action -> Game -> m ()
game actionB initGame = do
  rec -- :: Dynamic Game
    gDyn <- foldDyn ($) initGame $ (flip <$> actionB) <@> clickE
    -- :: Event Coordinate
    clickE <- (dyn $ board <$> gDyn) >>= switchPromptly never
  return ()

aGame :: MonadWidget t m => Behavior t Action -> (Int, Difficulty) -> m ()
aGame actionB (seed, difficulty@(size, _)) =
  andThen (board (emptyGame size)) (game actionB . makeGame difficulty seed)


-- Utilities:

-- from https://wiki.haskell.org/Case
case' :: a -> [(Bool, a)] -> a
case' def = maybe def snd . find fst

css :: [(String,String)] -> Text
css = pack . foldl (++) "" . Data.List.intersperse ";" . map (\ (a,v) -> a ++ ":" ++ v)

andThen :: MonadWidget t m => m (Event t a) -> (a -> m b) -> m ()
andThen start next = do
  -- andThenDyn :: Dynamic (Event t a)
  rec andThenDyn <- widgetHold start nextE
      -- nextE :: Event t (m (Event t a))
      let nextE = (never <$) <$> next <$> switchPromptlyDyn andThenDyn
  return ()
