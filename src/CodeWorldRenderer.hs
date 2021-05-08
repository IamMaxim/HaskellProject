{-# LANGUAGE OverloadedStrings #-}

module CodeWorldRenderer where

import CodeWorld hiding (Vector)
import Control.Monad
import qualified Data.Map as Map
import Data.Vector
import World

renderRange = 20

-- | Defines a renderable object.
class Drawable a where
  -- | renders a tile based on current time and world state. No transformations
  -- should be applied inside.
  draw :: Double -> World -> a -> Picture

translate :: Coords -> Picture -> Picture
translate (cx, cy) = translated (fromIntegral cx) (fromIntegral cy)

instance Drawable Tile where
  draw t w Void = blank
  draw t w (Ground color) = colored color (solidRectangle 1 1)
  draw t w (Wall color) = colored color (solidRectangle 0.9 0.9)

instance Drawable Entity where
  draw t w entity = (lettering . symbol) entity

instance Drawable World where
  draw t w world =
    playerPicture <> entitiesPicture <> tilesPicture
    where
      assocs = Prelude.zip filteredChunkCoords (Prelude.map (\coords -> chunks world Map.! coords) filteredChunkCoords)

      drawEnt ent = translate (pos ent `sub` centerPos) (draw t w ent)

      playerPicture = draw t w (player world)
      entitiesPicture =
        Prelude.foldMap drawEnt (entities world)

      tilesPicture =
        Prelude.foldMap
          (\(coords, chunk) -> translate ((coords `mul` chunkSize) `sub` centerPos) (draw t w chunk))
          assocs

      centerPos = (pos . player) world
      filteredChunkCoords :: [Coords]
      filteredChunkCoords =
        Prelude.filter
          (not . culled)
          ((Map.keys . chunks) world)

      -- True if given coords is not in render range and should be culled
      culled :: Coords -> Bool
      culled (x, y) =
        x - cx - chunkSize < renderRange
          && x - cx + chunkSize > renderRange
          && y - cy - chunkSize < renderRange
          && y - cy + chunkSize > renderRange
        where
          (cx, cy) = centerPos

instance Drawable Chunk where
  draw t w chunk = drawTiles t w (tiles chunk) <> drawTiles t w (backgroundTiles chunk)
    where
      drawTiles :: Double -> World -> Vector (Vector Tile) -> Picture
      drawTiles t w tiles =
        let tile2D = Prelude.map toList (toList tiles)
            tilesDrawn =
              [ let pic = draw t w tile
                 in translated x y pic
                | (y, tileRow) <- Prelude.zip [0 ..] tile2D,
                  (x, tile) <- Prelude.zip [0 ..] tileRow
              ]
         in pictures tilesDrawn