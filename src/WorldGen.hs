{-# LANGUAGE OverloadedStrings #-}

-- | This module is responsible for creating/generating the world.
module WorldGen where

import CodeWorld hiding (Vector)
import Control.Monad
import Data.Vector
import Data.Map
import System.Random
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
        entities = [],
        time = 0
      }

-- | Generates a (for now) stub chunk of ground tiles.
genChunk :: Coords -> IO Chunk
genChunk _ = do
  r <- randomIO :: IO Double
  g <- randomIO :: IO Double
  b <- randomIO :: IO Double
  let color = RGB r g b
  -- backgroundTiles <-
  --   newArray
  --     ((0, 0), (chunkSize - 1, chunkSize - 1))
  --     (Ground color) ::
  --     IO (IOArray (Int, Int) Tile)
  -- tiles <-
  --   newArray
  --     ((0, 0), (chunkSize - 1, chunkSize - 1))
  --     Void ::
  --     IO (IOArray (Int, Int) Tile)
  return
    Chunk
      { backgroundTiles = genTilesArray chunkSize (Ground color),
        tiles = genTilesArray chunkSize Void
      }

genTilesArray :: Int -> Tile -> Vector (Vector Tile)
genTilesArray n t = Data.Vector.replicate n (Data.Vector.replicate n t)

-- | Generates additional chunks as needed if there are not enough of them in the player range
updateChunks :: Coords -> World -> IO World
updateChunks coords world =
  Control.Monad.foldM update world chunksToCheck
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
