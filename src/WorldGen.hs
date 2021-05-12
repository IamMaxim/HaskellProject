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
  let chunk = genChunk (0, 0)
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
genChunk :: Coords -> Chunk
genChunk chunkCoords = do
  Chunk
    { backgroundTiles = perlinizeChunk chunkCoords (genTilesArray chunkSize Void),
      tiles = genTilesArray chunkSize Void
    }

genTilesArray :: Int -> Tile -> Vector (Vector Tile)
genTilesArray n t = Data.Vector.replicate n (Data.Vector.replicate n t)

-- | Generates additional chunks as needed if there are not enough of them in the player range
updateChunks :: Coords -> World -> World
updateChunks coords world =
  Prelude.foldr update world chunksToCheck
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
    update :: Coords -> World -> World
    update coords world = case Data.Map.lookup coords (chunks world) of
      Just chunk -> world
      Nothing -> world {chunks = insert coords (genChunk coords) (chunks world)}

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
              sampleTile (noiseAt (cx * chunkSize + x, cy * chunkSize + y))
          )
          ys
    )
    (indexedTiles tiles)

indexedTiles :: Vector (Vector Tile) -> Vector (Int, Vector (Int, Tile))
indexedTiles = indexed . Data.Vector.map indexed

deindexedTiles :: Vector (Int, Vector (Int, Tile)) -> Vector (Vector Tile)
deindexedTiles = Data.Vector.map (\(x, xs) -> Data.Vector.map snd xs)

--------------------------------------------------------------------------------
-- Converting noise value into tiles
--------------------------------------------------------------------------------

waterLevel = -0.9

sandLevel = -0.7

stoneLevel = 0.8

sampleTile :: Double -> Tile
sampleTile value
  | value <= waterLevel = Water
  | value <= sandLevel = Ground (RGB 1 0.84 0.4)
  | value <= stoneLevel = Ground (grassColor value)
  | otherwise = Ground (RGB 0.42 0.42 0.42)

grassColor :: Double -> Color
grassColor value = RGB (0.42 - value / 20) (0.65 - value / 10) 0.25
