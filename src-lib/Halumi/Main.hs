{-# LANGUAGE OverloadedRecordDot #-}

module Halumi.Main where

import Control.Monad
import UI.HSCurses.Curses

data Buffer = Buffer
  { content :: String
  , pos :: Integer
  }

instance Show Buffer where
  show buf = mconcat ["  pos: ", show buf.pos, "\n  content:\n" <> buf.content]

fromFile :: FilePath -> IO Buffer
fromFile path = do
  contents <- readFile path
  pure $ Buffer contents 0

main :: IO ()
main = do
  buf <- fromFile "LICENSE"
  win <- initScr
  keypad win True
  echo False
  wclear win
  mainLoop win buf
  endWin

mainLoop :: Window -> Buffer -> IO Buffer
mainLoop win buf = do
  wAddStr win buf.content
  wAddStr win "press something\n"
  refresh
  c <- getCh
  case c of
    KeyChar 'q' -> error "aus"
    KeyUp -> wAddStr win "upppp\n" >> refresh >> mainLoop win buf
    KeyDown -> wAddStr win "down pressed\n" >> refresh >> mainLoop win buf
    KeyLeft -> wAddStr win "left pressed\n" >> refresh >> mainLoop win buf
    KeyRight -> wAddStr win "right pressed\n" >> refresh >> mainLoop win buf
    _ -> wAddStr win "  other\n" >> refresh >> mainLoop win buf
