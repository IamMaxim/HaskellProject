

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Game where

import World
import CodeWorld
import WorldGen
import CodeWorldRenderer
import Player
import System.IO.Unsafe (unsafePerformIO)


gameMain :: IO ()
gameMain = do
  world <- genWorld
  -- Update the world once to generate more chunks around player
  world <- updateChunks ((pos . player) world) world
  activityOf world (\e w -> unsafePerformIO (handleInput e w)) (unsafePerformIO . drawWorld)

drawWorld :: World -> IO Picture
drawWorld world = draw (time world) world world

handleInput :: Event -> World -> IO World
handleInput (TimePassing t) w = return $ w { time = t }
handleInput e w = do
  newPlayer <- move e w (player w)
  return $ w { player = newPlayer }
