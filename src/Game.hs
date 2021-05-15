{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Game where

import CodeWorld
import CodeWorldRenderer
import Data.Text (pack, unpack)
import Inventory
import Player
import System.IO.Unsafe (unsafePerformIO)
import World
import WorldGen

gameMain :: IO ()
gameMain =
  let world = genWorld
  -- Update the world once to generate more chunks around player
  -- world <- updateChunks ((pos . player) world) world
  in activityOf (updateChunks ((pos . player) world) world) handleInput drawWorld

-- let inventory = createTestInventory :: Inventory TestItem
-- drawingOf (draw 0.0 world inventory)

drawWorld :: World -> Picture
drawWorld world = draw (time world) world world

handleBlockChanging :: Event -> World -> World
handleBlockChanging (PointerRelease (px, py)) world
  | x == 0 && y == 0 = world -- clicked on itsself
  | otherwise = (updateTileAt world clickedTileCoords newTile) {player = newPlayer}
  where
    x = round px
    y = round py
    currentPlayer = player world
    (playerX, playerY) = pos currentPlayer
    clickedTileCoords = (x + playerX, y + playerY)

    clickedTile = tileAt world clickedTileCoords

    playerInventory = inventory currentPlayer

    selectedInventoryItem = getActiveItem playerInventory

    (newTile, newInventory) = case clickedTile of
      Void -> case selectedInventoryItem of -- build
        Nothing -> (clickedTile, playerInventory) -- cannot build from active cell
        Just item -> (item, removeItem playerInventory item)
      tile -> (Void, addItem playerInventory tile)

    newPlayer = currentPlayer {inventory = newInventory}
handleBlockChanging _ world = world

handleActiveInventorySwitch :: Event -> World -> World
handleActiveInventorySwitch (KeyPress btn) world
  | "1" <= btn && btn <= "9" =
    let targetSlot :: Int
        targetSlot = (read . unpack) btn - 1
        playerInventory = (inventory . player) world
        newPlayerInventory = changeActiveItem playerInventory targetSlot
        newPlayer = (player world) {inventory = newPlayerInventory}
     in world {player = newPlayer}
  | otherwise = world
handleActiveInventorySwitch _ world = world

worldHandlers :: [Event -> World -> World]
worldHandlers = [handleBlockChanging, handleActiveInventorySwitch, movePlayer]

handleInput :: Event -> World -> World
handleInput (TimePassing t) w = w {time = time w + t}
handleInput e w = foldl (\currentWorld handler -> handler e currentWorld) w worldHandlers
