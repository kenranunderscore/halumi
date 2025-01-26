module Halumi.Buffer (
  Buffer (content, lines, pos),
  Pos (..),
  currentLine,
  line,
  moveDown,
  moveLeft,
  moveRight,
  moveUp,
  newBuffer,
  topLeft,
)
where

import Data.List ((!?))
import Data.Maybe qualified as Maybe

data Pos = Pos
  { row :: Int
  , col :: Int
  }
  deriving stock (Show, Eq)

topLeft :: Pos
topLeft = Pos 0 0

data Buffer = Buffer
  { content :: String
  , lines :: [String]
  , pos :: Pos
  }

newBuffer :: String -> Buffer
newBuffer content =
  Buffer
    { content
    , lines = lines content
    , pos = topLeft
    }

line :: Int -> Buffer -> Maybe String
line n buf = buf.lines !? n

currentLine :: Buffer -> String
currentLine buf = Maybe.fromJust $ line buf.pos.row buf

xmax :: Buffer -> Int
xmax buf = length (currentLine buf) - 1

ymax :: Buffer -> Int
ymax buf = length buf.lines - 1

instance Show Buffer where
  show buf = mconcat ["  pos: ", show buf.pos, "\n  content:\n" <> buf.content]

updatePos :: (Pos -> Pos) -> Buffer -> Buffer
updatePos f buf = buf{pos = f buf.pos}

moveLeft :: Buffer -> Buffer
moveLeft = updatePos (\(Pos y x) -> Pos y $ max (x - 1) 0)

moveRight :: Int -> Buffer -> Buffer
moveRight width buf =
  let m = max (min (width - 1) (xmax buf)) 0
  in updatePos (\(Pos y x) -> Pos y $ min (x + 1) m) buf

moveUp :: Buffer -> Buffer
moveUp buf =
  let
    ynext = (max (buf.pos.row - 1) 0)
    xnext = case line ynext buf of
      Nothing -> buf.pos.col
      Just l -> min buf.pos.col (length l)
  in
    updatePos (const $ Pos ynext xnext) buf

moveDown :: Int -> Buffer -> Buffer
moveDown height buf =
  let
    m = min (height - 1) (ymax buf)
    ynext = min (buf.pos.row + 1) m
    xnext = case line ynext buf of
      Nothing -> buf.pos.col
      Just l -> min buf.pos.col (length l)
  in
    updatePos (const $ Pos ynext xnext) buf
