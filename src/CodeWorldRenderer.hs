{-# LANGUAGE OverloadedStrings #-}

module CodeWorldRenderer where

import CodeWorld
import Data.Array.IO
import qualified Data.Map as Map
import World
import Control.Monad

renderRange = 20

voidColor = RGB 0.1 0.1 0.1

-- | Defines a renderable object.
class Drawable a where
  -- | renders a tile based on current time and world state. No transformations
  -- should be applied inside.
  draw :: Double -> World -> a -> IO Picture

translate :: Coords -> Picture -> Picture
translate (cx, cy) = translated (fromIntegral cx) (fromIntegral cy)

instance Drawable Tile where
  draw t w Void = return $ colored voidColor (rectangle 1 1)
  draw t w (Ground color) = return $ colored color (rectangle 1 1)
  draw t w (Wall color) = return $ colored color (rectangle 0.9 0.9)

instance Drawable Entity where
  draw t w entity = return $ (lettering . symbol) entity

instance Drawable World where
  draw t w world = do
    let assocs = zip filteredChunkCoords (map (\coords -> chunks world Map.! coords) filteredChunkCoords)
    pictures <- mapM
      (\(coords, chunk) ->
        draw t w chunk >>= (return . translate coords))
      assocs
    return $ foldr (<>) blank pictures
    -- pics <- mapM (\coords -> (coords, draw t w (chunks world Map.! coords))) filteredChunkCoords :: IO (Coords, Picture)
--     foldMap
--       ( \(coords, p) ->
--           translate
--             coords
--             p
--       )
--       pics
    where
      centerPos = (pos . player) world
      filteredChunkCoords :: [Coords]
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
  draw t w chunk = do
    assocs <- ((getAssocs . tiles) chunk :: IO [((Int, Int), Tile)])
    pictures <- mapM
      (\(coords, tile) ->
         draw t w tile >>= return . translate coords)
      assocs
    return $ foldr (<>) blank pictures
