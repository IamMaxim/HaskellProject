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

instance Drawable Tile where
  draw t w Void = colored voidColor (rectangle 1 1)
  draw t w (Ground color) = colored color (rectangle 1 1)
  draw t w (Wall color) = colored color (rectangle 0.9 0.9)


-- | Entity is something "living" (or at least able to move freely).
data Entity = Entity
  { pos :: Coords,
    name :: String,
    symbol :: Text
  }

instance Drawable Entity where
  draw t w entity = (lettering . symbol) entity


-- | 32x32 set of tiles. Used as a atomic unit of world generation/loading.
data Chunk = Chunk
  { tiles :: Array (Int, Int) Tile
  }

instance Drawable Chunk where
  draw t w chunk = lettering "amma chunk"

-- | State of the entire game world.
data World = World
  { chunks :: Map.Map (Int, Int) Chunk,
    entities :: [Entity],
    player :: Entity
  }

renderRange = 20

instance Drawable World where
  draw t w world =
    foldMap
      (\coords -> translate coords
        (draw t w (chunks world Map.! coords)))
      filteredChunkCoords
    where
      centerPos = (pos . player) world
      filteredChunkCoords =
        filter
          (not . culled)
          ((Map.keys . chunks) world)

      -- True if given coords is not in render range and should be culled
      culled :: Coords -> Bool
      culled (x, y) =
        x - cx - 16 < renderRange
          && x - cx + 16 > renderRange
          && y - cy - 16 < renderRange
          && y - cy + 16 > renderRange
        where
          (cx, cy) = centerPos



---------------------------------------------------------------------------------

-- | Defines a renderable object.
class Drawable a where
  -- | renders a tile based on current time and world state. No transformations
  -- should be applied inside.
  draw :: Double -> World -> a -> Picture


translate :: Coords -> Picture -> Picture
translate (cx, cy) = translated (fromIntegral cx) (fromIntegral cy)
