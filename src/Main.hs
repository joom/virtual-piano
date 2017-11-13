{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Control.Monad (forM_)
import Data.List (partition)
import Data.Maybe (catMaybes)
import Euterpea
import UI.NCurses

data Options = Options {
    range          :: [AbsPitch]
  , firstKeyRow    :: Integer
  , firstKeyColumn :: Integer
  , whiteKeyWidth  :: Integer
  , whiteKeyHeight :: Integer
  , blackKeyWidth  :: Integer
  , blackKeyHeight :: Integer
  }

initialOptions :: Options
initialOptions = Options [48..84] 5 5 6 10 4 6

data Toolbox = Toolbox {
    whiteKeyColor   :: ColorID
  , blackKeyColor   :: ColorID
  , pressedKeyColor :: ColorID
  }

data PianoKey = PianoKey {
    absP    :: AbsPitch
  , name    :: String
  , isBlack :: Bool
  , column  :: Integer
  , pressed :: Bool
  } deriving Show

keyName :: AbsPitch -> String
keyName = renameSharp . f . pitch
  where f (x, y) = show x ++ show y
        renameSharp = map $ \case 's' -> '#' ; c -> c

isBlackKey :: AbsPitch -> Bool
isBlackKey i = fst (pitch i) `elem` [As,Cs,Ds,Fs,Gs]

keys :: Options -> [PianoKey]
keys Options{..} = catMaybes $ scanl f Nothing range
  where
    f Nothing i = Just $ PianoKey i (keyName i) (isBlackKey i) firstKeyRow False
    f (Just PianoKey{..}) i = Just $ PianoKey i (keyName i) (isBlackKey i) c False
      where c = case (isBlack, isBlackKey i) of -- are the prev and curr keys black
                  (False, True)  -> column + (div whiteKeyWidth 2) + 1
                  (False, False) -> column + whiteKeyWidth
                  (True, False)  -> column + (div blackKeyWidth 2)
                  _ -> error "Impossible: Two black keys cannot be consecutive"

drawKey :: Options -> Toolbox -> PianoKey -> Update ()
drawKey Options{..} Toolbox{..} PianoKey{..} = do
  let width = if isBlack then blackKeyWidth else whiteKeyWidth
  let height = if isBlack then blackKeyHeight else whiteKeyHeight
  let r = firstKeyRow
  let c = column
  setColor $ if pressed then pressedKeyColor else
             if isBlack then blackKeyColor else whiteKeyColor
  -- draw the background
  moveCursor r c
  forM_ [r..(r + height)] $ \n -> do
    moveCursor n c
    drawString $ replicate (fromInteger width) ' '
  -- draw the borders of the box
  setColor $ if isBlack then blackKeyColor else whiteKeyColor
  moveCursor r c            ; drawLineH (Just glyphLineH) width
  moveCursor (r + height) c ; drawLineH (Just glyphLineH) width
  moveCursor r c            ; drawLineV (Just glyphLineV) height
  moveCursor r (c + width)  ; drawLineV (Just glyphLineV) height
  moveCursor (r + height) c           ; drawGlyph glyphCornerLL
  moveCursor r (c + width)            ; drawGlyph glyphCornerUR
  moveCursor (r + height) (c + width) ; drawGlyph glyphCornerLR
  moveCursor r c                      ; drawGlyph glyphCornerUL

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  toolbox <- Toolbox <$> newColorID ColorBlack ColorWhite 1
                     <*> newColorID ColorWhite ColorBlack 2
                     <*> newColorID ColorBlack ColorYellow 3
  let options = initialOptions
  let (blacks, whites) = partition isBlack (keys options)
  updateWindow w $ do
    -- draw white keys first
    forM_ whites $ \key@(PianoKey{..}) -> do
      drawKey options toolbox key
      moveCursor (5 + whiteKeyHeight options - 1) (column + 1)
      drawString name
    -- draw black keys over the white keys
    forM_ blacks $ \key@(PianoKey{..}) -> do
      drawKey options toolbox key
    moveCursor 0 0
  render
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
