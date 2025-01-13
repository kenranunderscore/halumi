{-# LANGUAGE OverloadedRecordDot #-}

module Halumi.Main where

import UI.HSCurses.Curses

type Pos = Int

data Buffer = Buffer
  { content :: String
  , pos :: Pos
  }

instance Show Buffer where
  show buf = mconcat ["  pos: ", show buf.pos, "\n  content:\n" <> buf.content]

updatePos :: (Pos -> Pos) -> Buffer -> Buffer
updatePos f buf = buf{pos = f buf.pos}

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
  _ <- mainLoop win buf
  wclear win
  endWin

moveLeft :: Buffer -> Buffer
moveLeft = updatePos (\p -> max (p - 1) 0)

moveRight :: Buffer -> Buffer
moveRight buf = updatePos (\p -> min (p + 1) (length buf.content - 1)) buf

mainLoop :: Window -> Buffer -> IO Buffer
mainLoop win buf = do
  wclear win
  wAddStr win buf.content
  move 0 buf.pos
  refresh
  c <- getCh
  case c of
    KeyChar 'q' -> error "aus"
    KeyUp -> mainLoop win buf
    KeyDown -> mainLoop win buf
    KeyLeft -> do
      move 0 buf.pos
      refresh
      mainLoop win (moveLeft buf)
    KeyRight -> do
      move 0 buf.pos
      refresh
      mainLoop win (moveRight buf)
    _ -> mainLoop win buf
