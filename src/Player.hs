{-# LANGUAGE OverloadedStrings #-}

module Player where

import CodeWorld
import World

data MoveDirection = MoveUp | MoveDown | MoveLeft | MoveRight

move :: Event -> World -> Entity -> Entity
move event world entity = case dir of
  Nothing -> entity
  Just d ->
    case targetTile of
      Void -> entity {pos = newPos d (pos entity)}
      _ -> entity
    where
      targetTile = tileAt world (newPos d (pos entity))
  where
    dir = eventToDirection event

eventToDirection :: Event -> Maybe MoveDirection
eventToDirection (KeyPress "W") = Just MoveUp
eventToDirection (KeyPress "A") = Just MoveLeft
eventToDirection (KeyPress "S") = Just MoveDown
eventToDirection (KeyPress "D") = Just MoveRight
eventToDirection _ = Nothing

newPos :: MoveDirection -> Coords -> Coords
newPos MoveUp (x, y) = (x, y + 1)
newPos MoveDown (x, y) = (x, y - 1)
newPos MoveLeft (x, y) = (x - 1, y)
newPos MoveRight (x, y) = (x + 1, y)
