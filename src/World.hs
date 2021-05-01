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
chunkSize = 32

halfChunkSize :: Int
halfChunkSize = chunkSize `div` 2

type Coords = (Int, Int)

-- | Tile is a solid 1x1 unit that is strictly tied to the grid.
data Tile = Void | Ground Color | Wall Color

voidColor = RGB 0.1 0.1 0.1

-- | Entity is something "living" (or at least able to move freely).
data Entity = Entity
  { pos :: Coords,
    name :: String,
    symbol :: Text
  }

-- | 32x32 set of tiles. Used as a atomic unit of world generation/loading.
data Chunk = Chunk
  { tiles :: IOArray (Int, Int) Tile
  }

-- | State of the entire game world.
data World = World
  { chunks :: Map.Map (Int, Int) Chunk,
    entities :: [Entity],
    player :: Entity
  }
