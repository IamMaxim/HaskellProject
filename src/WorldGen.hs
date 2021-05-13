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
    { backgroundTiles =
        perlinizeChunkBackground
          chunkCoords
          (genTilesArray chunkSize Void),
      tiles =
        perlinizeChunk putStones perlinObj chunkCoords
          (genTilesArray chunkSize Void)
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

perlinObj2 :: Int -> Perlin
perlinObj2 = Perlin 0.1 1 64 1

-- | Generates a noise value for the given coords.
noiseAt :: (Int -> Perlin) -> Coords -> Double
noiseAt perlin (x, y) =
  case getNoiseValue (perlin 123) [] (fromIntegral x, fromIntegral y, 0) of
    Nothing -> -1
    Just d -> d

perlinizeChunk ::
  -- | Sample function for each tile
  (Double -> Tile -> Tile) ->
  -- | Perlin noise generator
  (Int -> Perlin) ->
  -- | Chunk coords
  Coords ->
  -- | Initial chunk tiles
  Vector (Vector Tile) ->
  Vector (Vector Tile)
perlinizeChunk sampleFunc perlin = mapTiles (\(x, y) -> sampleFunc (noiseAt perlin (x, y)))

perlinizeChunkBackground ::
  -- | Chunk coords
  Coords ->
  -- | Initial chunk tiles
  Vector (Vector Tile) ->
  Vector (Vector Tile)
perlinizeChunkBackground = perlinizeChunk sampleBackgroundTile perlinObj

-- perlinizeChunkForeground :: Coords -> Vector (Vector Tile) -> Vector (Vector Tile)
-- perlinizeChunkForeground = perlinizeChunk sampleForegroundTile perlinObj2

indexedTiles :: Vector (Vector Tile) -> Vector (Int, Vector (Int, Tile))
indexedTiles = indexed . Data.Vector.map indexed

mapTiles :: ((Int, Int) -> Tile -> Tile) -> Coords -> Vector (Vector Tile) -> Vector (Vector Tile)
mapTiles f (cx, cy) tiles =
  Data.Vector.map
    ( \(y, ys) ->
        Data.Vector.map
          ( \(x, tile) ->
              f (cx * chunkSize + x, cy * chunkSize + y) tile
          )
          ys
    )
    (indexedTiles tiles)

-- deindexedTiles :: Vector (Int, Vector (Int, Tile)) -> Vector (Vector Tile)
-- deindexedTiles = Data.Vector.map (\(x, xs) -> Data.Vector.map snd xs)

--------------------------------------------------------------------------------
-- Converting noise value into tiles
--------------------------------------------------------------------------------

waterLevel = -0.9

sandLevel = -0.7

stoneLevel = 0.8

sandColor :: Color
sandColor = RGB 1 0.84 0.4

stoneColor :: Color 
stoneColor = RGB 0.7 0.7 0.7
-- stoneColor = RGB 0.42 0.42 0.42

sampleBackgroundTile :: Double -> Tile -> Tile
sampleBackgroundTile value tile
  | value <= waterLevel = Water
  | value <= sandLevel = Ground sandColor
  | value <= stoneLevel = Ground (grassColor value)
  | otherwise = Ground (darker 0.2 stoneColor)

grassColor :: Double -> Color
grassColor value = RGB (0.42 - value / 20) (0.65 - value / 10) 0.25

putStones :: Double -> Tile -> Tile
putStones value tile
  | value >= stoneLevel = Ground stoneColor
  | otherwise = tile

-- sampleForegroundTile :: Double -> Tile -> Tile
-- sampleForegroundTile value tile
--   | value >= stoneLevel = Ground (RGB 0.70 0.70 0.70)
--   | value <= -0.95 = Wall (RGB 0.61 0.37 0.14)
--   | otherwise = tile
