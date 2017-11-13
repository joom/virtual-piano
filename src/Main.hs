{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (partition, find)
import Data.Maybe (catMaybes)
import Euterpea
import UI.NCurses

data Options = Options {
    range          :: [AbsPitch]
  , controls       :: [Char]
  , firstKeyRow    :: Integer
  , firstKeyColumn :: Integer
  , whiteKeyWidth  :: Integer
  , whiteKeyHeight :: Integer
  , blackKeyWidth  :: Integer
  , blackKeyHeight :: Integer
  , windowRows     :: Integer
  , windowCols     :: Integer
  }

initialOptions :: Options
initialOptions = Options [48..88]
                         "1234567890-=\bqwertyuiop[]\\asdfghjkl;'\nzxcvbnm,./"
                         0 0 6 10 4 6 0 0

data Toolbox = Toolbox {
    whiteKeyColor   :: ColorID
  , blackKeyColor   :: ColorID
  , pressedKeyColor :: ColorID
  }

data PianoKey = PianoKey {
    absP    :: AbsPitch
  , control :: Char
  , name    :: String
  , isBlack :: Bool
  , column  :: Integer
  , pressed :: Bool
  } deriving Show

data State = State {
    keysState :: [Key]
  }

keyName :: AbsPitch -> String
keyName = renameSharp . f . pitch
  where f (x, y) = show x ++ show y
        renameSharp = map $ \case 's' -> '#' ; c -> c

showControl :: Char -> String
showControl '\n' = "↵"
showControl '\\' = "\\"
showControl '\'' = "'"
showControl '\b' = "⌫"
showControl c = init $ tail $ show $ c

isBlackKey :: AbsPitch -> Bool
isBlackKey i = fst (pitch i) `elem` [As,Cs,Ds,Fs,Gs]

keys :: Options -> [PianoKey]
keys Options{..} = catMaybes $ scanl f Nothing (zip range controls)
  where
    f Nothing (i, c) = Just $ PianoKey i c (keyName i) (isBlackKey i) firstKeyColumn False
    f (Just PianoKey{..}) (i, c) = Just $ PianoKey i c (keyName i) (isBlackKey i) col False
      where col = case (isBlack, isBlackKey i) of -- are the prev and curr keys black
                    (False, True)  -> column + (div whiteKeyWidth 2) + 1
                    (False, False) -> column + whiteKeyWidth
                    (True, False)  -> column + (div blackKeyWidth 2)
                    _ -> error "Impossible: Two black keys cannot be consecutive"

initialState :: State
initialState = State []

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

waitFor :: Window -> (Event -> Curses Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> do
              b <- p ev'
              if b then return () else loop

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  toolbox <- Toolbox <$> newColorID ColorBlack ColorWhite 1
                     <*> newColorID ColorWhite ColorBlack 2
                     <*> newColorID ColorBlack ColorYellow 3
  let options = initialOptions
  updateWindow w $ do
    (sizeR, sizeC) <- windowSize
    let options = initialOptions {
        windowRows = sizeR
      , windowCols = sizeC
      }
    let (blacks, whites) = partition isBlack (keys options)
    let whiteCount = fromIntegral (length whites)
    let options = initialOptions {
        firstKeyRow = (div sizeR 2) - (div (whiteKeyHeight options) 2)
      , firstKeyColumn = (div sizeC 2) - (div (whiteKeyWidth options * whiteCount) 2)
      }
    -- update the locations for the keys
    let (blacks, whites) = partition isBlack (keys options)
    -- draw white keys first
    forM_ whites $ \key@(PianoKey{..}) -> do
      drawKey options toolbox key
      moveCursor (firstKeyRow options + whiteKeyHeight options - 2) (column + 1)
      drawString name
      moveCursor (firstKeyRow options + whiteKeyHeight options - 1) (column + 1)
      drawString $ showControl control
    -- draw black keys over the white keys
    forM_ blacks $ \key@(PianoKey{..}) -> do
      drawKey options toolbox key
      moveCursor (firstKeyRow options + blackKeyHeight options - 2) (column + 1)
      drawString name
      moveCursor (firstKeyRow options + blackKeyHeight options - 1) (column + 1)
      drawString $ showControl control
    moveCursor 0 0
  render
  waitFor w $ \ev -> do
    case ev of
      EventCharacter c -> do
        case find ((== c) . control) (keys options) of
          Nothing -> return ()
          Just key -> do
            liftIO $ forkIO $ play  $ note 1 (absP key)
            return ()
      _ -> return ()
    return $ ev `elem` map EventCharacter ['\ESC', '`']
