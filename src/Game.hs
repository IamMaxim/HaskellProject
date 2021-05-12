{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Game where

import CodeWorld
import CodeWorldRenderer
import Player
import System.IO.Unsafe (unsafePerformIO)
import World
import WorldGen
import Inventory

gameMain :: IO ()
gameMain = do
  world <- genWorld
  -- Update the world once to generate more chunks around player
  -- world <- updateChunks ((pos . player) world) world
  activityOf (updateChunks ((pos . player) world) world) handleInput drawWorld
  -- let inventory = createTestInventory :: Inventory TestItem
  -- drawingOf (draw 0.0 world inventory)

drawWorld :: World -> Picture
drawWorld world = draw (time world) world world

handleInput :: Event -> World -> World
handleInput (TimePassing t) w = w {time = t}
handleInput e w = (updateChunks (pos (player w)) w) {player = newPlayer}
  where
    newPlayer = move e w (player w)
