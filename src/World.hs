{-# LANGUAGE OverloadedStrings #-}

-- | Contains the data structure and functions used to manipulate the world.
module World where

import CodeWorld
import Data.Array
import Data.Array.IO
import Data.Array.MArray
import qualified Data.Map as Map
import Data.Text hiding (filter)

chunkSize :: Int
chunkSize = 8

halfChunkSize :: Int
halfChunkSize = chunkSize `div` 2

type Coords = (Int, Int)

mul :: Coords -> Int -> Coords
mul (x, y) mult = (x * mult, y * mult)

-- | Tile is a solid 1x1 unit that is strictly tied to the grid.
data Tile = Void | Ground Color | Wall Color

-- | Entity is something "living" (or at least able to move freely).
data Entity = Entity
  { pos :: Coords,
    name :: String,
    symbol :: Text
  }

-- | 32x32 set of tiles. Used as a atomic unit of world generation/loading.
data Chunk = Chunk
  { backgroundTiles :: IOArray (Int, Int) Tile,
    tiles :: IOArray (Int, Int) Tile
  }

-- | State of the entire game world.
data World = World
  { chunks :: Map.Map (Int, Int) Chunk,
    entities :: [Entity],
    player :: Entity,
    time :: Double
  }

-- | Returns coords of the chunk that the tile is located in.
coordToChunkCoord :: Coords -> Coords
coordToChunkCoord (cx, cy) = (cx `div` chunkSize, cy `div` chunkSize)

-- | Returns the coordinates that are local to the chunk that the tile is
-- located in.
clampCoordToChunk :: Coords -> Coords
clampCoordToChunk (cx, cy) = (cx `mod` chunkSize, cy `mod` chunkSize)

-- | Computes a chunk for the given coord and extracts the tile at given
-- position.
tileAt :: World -> Coords -> IO Tile
tileAt world coords = case chunk of
  Nothing -> return Void
  Just c -> readArray (tiles c) (clampCoordToChunk coords)
  where
    chunk :: Maybe Chunk
    chunk = Map.lookup (coordToChunkCoord coords) (chunks world)
