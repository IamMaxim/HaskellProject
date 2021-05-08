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
  world <- updateChunks ((pos . player) world) world
  activityOf world handleInput drawWorld

drawWorld :: World -> Picture
drawWorld world = draw (time world) world world

handleInput :: Event -> World -> World
handleInput (TimePassing t) w = w {time = t}
handleInput e w = w {player = newPlayer}
  where
    newPlayer = move e w (player w)
