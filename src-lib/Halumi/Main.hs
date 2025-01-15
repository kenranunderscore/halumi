{-# LANGUAGE OverloadedRecordDot #-}

module Halumi.Main where

import UI.HSCurses.Curses

data Pos = Pos
  { row :: Int
  , col :: Int
  }
  deriving stock (Show, Eq)

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
  pure $ Buffer contents (Pos 0 0)

main :: IO ()
main = do
  buf <- fromFile "LICENSE"
  win <- initScr
  (h, w) <- scrSize
  keypad win True
  echo False
  _ <- mainLoop win w h buf
  wclear win
  endWin
  update

moveLeft :: Buffer -> Buffer
moveLeft = updatePos (\(Pos y x) -> Pos y $ max (x - 1) 0)

moveRight :: Int -> Buffer -> Buffer
moveRight width buf =
  updatePos (\(Pos y x) -> Pos y $ min (x + 1) (width - 1)) buf

moveUp :: Buffer -> Buffer
moveUp = updatePos (\(Pos y x) -> Pos (max (y - 1) 0) x)

moveDown :: Int -> Buffer -> Buffer
moveDown height buf =
  updatePos (\(Pos y x) -> Pos (min (y + 1) (height - 1)) x) buf

mainLoop :: Window -> Int -> Int -> Buffer -> IO Buffer
mainLoop win w h buf = do
  wclear win
  wAddStr win buf.content
  move buf.pos.row buf.pos.col
  refresh
  c <- getCh
  case c of
    KeyChar 'q' -> error "aus"
    KeyUp -> go (moveUp buf)
    KeyDown -> go (moveDown h buf)
    KeyLeft -> go (moveLeft buf)
    KeyRight -> go (moveRight w buf)
    _ -> go buf
 where
  go = mainLoop win w h
