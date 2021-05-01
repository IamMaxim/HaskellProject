{-# LANGUAGE OverloadedStrings #-}

-- | This module is responsible for creating/generating the world.
module WorldGen where

import CodeWorld
import Control.Monad
import Data.Array.IO
import Data.Array.MArray
import Data.Map
import World

-- | Generates the initial world state
genWorld :: IO World
genWorld = do
  chunk <- genChunk (0, 0)
  return
    World
      { chunks = singleton (0, 0) chunk,
        player =
          Entity
            { pos = (0, 0),
              name = "Player",
              symbol = "P"
            },
        entities = []
      }

genChunk :: Coords -> IO Chunk
genChunk _ = do
  tiles <-
    newArray
      ((0, 0), (chunkSize - 1, chunkSize - 1))
      (Ground (RGB 1 0 1)) ::
      IO (IOArray (Int, Int) Tile)
  return
    Chunk {tiles = tiles}

-- | Generates additional chunks as needed if there are not enough of them in the player range
updateChunks :: Coords -> World -> IO World
updateChunks coords world =
  foldM update world chunksToCheck
  where
    (px, py) = (coordToChunkCoord . pos . player) world
    chunksToCheck =
      [ (px - 1, py - 1),
        (px - 1, py),
        (px - 1, py + 1),
        (px, py - 1),
        (px, py),
        (px, py + 1),
        (px + 1, py - 1),
        (px + 1, py),
        (px + 1, py + 1)
      ]
    update :: World -> Coords -> IO World
    update = \world coords -> case Data.Map.lookup coords (chunks world) of
      Just chunk -> return world
      Nothing -> do
        chunk <- genChunk coords
        return world {chunks = insert coords chunk (chunks world)}
