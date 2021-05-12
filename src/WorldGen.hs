{-# LANGUAGE OverloadedStrings #-}

-- | This module is responsible for creating/generating the world.
module WorldGen where

import CodeWorld hiding (Vector)
import CodeWorldRenderer
import Control.Monad
import Data.Map
import Data.Maybe
import Data.Vector
import Inventory
import Math.Noise
import System.Random
import World

-- | Generates the initial world state
genWorld :: IO World
genWorld = do
  chunk <- genChunk (0, 0)
  return
    World
      { chunks = Data.Map.singleton (0, 0) chunk,
        player =
          Entity
            { pos = (0, 0),
              name = "Player",
              symbol = "P",
              inventory = emptyInventory
            },
        entities = [],
        time = 0
      }

-- | Generates a (for now) stub chunk of ground tiles.
genChunk :: Coords -> IO Chunk
genChunk chunkCoords = do
  -- r <- randomIO :: IO Double
  -- g <- randomIO :: IO Double
  -- b <- randomIO :: IO Double
  -- let color = RGB r g b
  return
    Chunk
      { backgroundTiles = perlinizeChunk chunkCoords (genTilesArray chunkSize Void),
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

--------------------------------------------------------------------------------
-- Perlin stuff
--------------------------------------------------------------------------------

-- | Returns the Perlin noise generator with predefined parameters
perlinObj ::
  -- | seed to generate noise from
  Int ->
  Perlin
perlinObj = Perlin 0.1 1 14 1

-- | Generates a noise value for the given coords.
noiseAt :: Coords -> Double
noiseAt (x, y) =
  case getNoiseValue (perlinObj 123) [] (fromIntegral x, fromIntegral y, 0) of
    Nothing -> -1
    Just d -> d

perlinizeChunk :: Coords -> Vector (Vector Tile) -> Vector (Vector Tile)
perlinizeChunk (cx, cy) tiles =
  Data.Vector.map
    ( \(y, ys) ->
        Data.Vector.map
          ( \(x, tile) ->
              Ground (RGB (noiseAt (cx * chunkSize + x, cy * chunkSize + y)) 0 0)
          )
          ys
    )
    (indexedTiles tiles)

indexedTiles :: Vector (Vector Tile) -> Vector (Int, Vector (Int, Tile))
indexedTiles = indexed . Data.Vector.map indexed

deindexedTiles :: Vector (Int, Vector (Int, Tile)) -> Vector (Vector Tile)
deindexedTiles = Data.Vector.map (\(x, xs) -> Data.Vector.map snd xs)
