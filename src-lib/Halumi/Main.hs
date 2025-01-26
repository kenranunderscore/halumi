module Halumi.Main where

import Halumi.Buffer
import UI.HSCurses.Curses

loadFile :: FilePath -> IO Buffer
loadFile path = do
  contents <- readFile path
  pure $ newBuffer contents

main :: IO ()
main = do
  buf <- loadFile "LICENSE"
  win <- initScr
  (h, w) <- scrSize
  keypad win True
  echo False
  _ <- mainLoop win w h buf
  wclear win
  endWin

mainLoop :: Window -> Int -> Int -> Buffer -> IO Buffer
mainLoop win w h buf = do
  wclear win
  wAddStr win buf.content
  move buf.pos.row buf.pos.col
  refresh
  c <- getCh
  case c of
    KeyChar 'q' -> pure buf
    KeyUp -> go (moveUp buf)
    KeyDown -> go (moveDown h buf)
    KeyLeft -> go (moveLeft buf)
    KeyRight -> go (moveRight w buf)
    _ -> go buf
 where
  go = mainLoop win w h
