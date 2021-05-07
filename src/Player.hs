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
      Void -> entity { pos = newPos d (pos entity) }
      _ -> entity
      where
        targetTile = tileAt world (newPos d (pos entity))
  where
    dir = eventToDirection event

eventToDirection :: Event -> Maybe MoveDirection
eventToDirection (KeyPress "w") = Just MoveUp
eventToDirection (KeyPress "a") = Just MoveLeft
eventToDirection (KeyPress "s") = Just MoveDown
eventToDirection (KeyPress "d") = Just MoveRight
eventToDirection _ = Nothing

-- -- | Generalized move function that accepts event to check if it is possible
-- -- to move to a particular cell and if possible, then updates the entity
-- -- position.
-- moveG :: (World -> Entity -> MoveDirection -> Bool) -> Event -> World -> Entity -> Entity
-- moveG (KeyPress "w") = checkAndMove MoveUp
-- moveG (KeyPress "a") = checkAndMove MoveLeft
-- moveG (KeyPress "s") = checkAndMove MoveDown
-- moveG (KeyPress "d") = checkAndMove MoveRight
--
-- checkAndMove :: MoveDirection -> (World -> Entity -> MoveDirection -> Bool) -> World -> Entity -> Entity
-- checkAndMove dir check world entity =
--   if check world entity dir
--     then entity {pos = newPos dir (pos entity)}
--     else entity

newPos :: MoveDirection -> Coords -> Coords
newPos MoveUp (x, y) = (x, y + 1)
newPos MoveDown (x, y) = (x, y - 1)
newPos MoveLeft (x, y) = (x - 1, y)
newPos MoveRight (x, y) = (x + 1, y)
