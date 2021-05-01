{-# LANGUAGE OverloadedStrings #-}

module CodeWorldRenderer where

import CodeWorld
import qualified Data.Map as Map
import World
    ( World(player, chunks),
      Chunk,
      Entity(pos, symbol),
      Tile(..),
      Coords,
      

renderRange = 20
voidColor = RGB 0.1 0.1 0.1

-- | Defines a renderable object.
class Drawable a where
  -- | renders a tile based on current time and world state. No transformations
  -- should be applied inside.
  draw :: Double -> World -> a -> Picture

translate :: Coords -> Picture -> Picture
translate (cx, cy) = translated (fromIntegral cx) (fromIntegral cy)

instance Drawable Tile where
  draw t w Void = colored voidColor (rectangle 1 1)
  draw t w (Ground color) = colored color (rectangle 1 1)
  draw t w (Wall color) = colored color (rectangle 0.9 0.9)

instance Drawable Entity where
  draw t w entity = (lettering . symbol) entity

instance Drawable World where
  draw t w world =
    foldMap
      ( \coords ->
          translate
            coords
            (draw t w (chunks world Map.! coords))
      )
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

instance Drawable Chunk where
  draw t w chunk = lettering "amma chunk"