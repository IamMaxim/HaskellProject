{-# LANGUAGE OverloadedStrings #-}

-- | Contains the data structure and functions used to manipulate the world.
module World where

import CodeWorld
import Data.Array
import qualified Data.Map as Map
import Data.Text hiding (filter)

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
  { tiles :: Array (Int, Int) Tile
  }

-- | State of the entire game world.
data World = World
  { chunks :: Map.Map (Int, Int) Chunk,
    entities :: [Entity],
    player :: Entity
  }