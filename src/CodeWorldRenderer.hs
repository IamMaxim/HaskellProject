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
  -- draw t w Void = return $ colored voidColor (solidRectangle 1 1)
  draw t w Void = return blank
  draw t w (Ground color) = return $ colored color (solidRectangle 1 1)
  draw t w (Wall color) = return $ colored color (solidRectangle 0.9 0.9)

instance Drawable Entity where
  draw t w entity = return $ (lettering . symbol) entity

instance Drawable World where
  draw t w world = do
    let assocs = zip filteredChunkCoords (map (\coords -> chunks world Map.! coords) filteredChunkCoords)
    pictures <- mapM
      (\(coords, chunk) ->
        draw t w chunk >>= (return . translate (coords `mul` chunkSize)))
      assocs
    let tilesPicture = foldr (<>) blank pictures

    -- TODO: implement entities drawing

    return tilesPicture
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
        x - cx - chunkSize < renderRange
          && x - cx + chunkSize > renderRange
          && y - cy - chunkSize < renderRange
          && y - cy + chunkSize > renderRange
        where
          (cx, cy) = centerPos

instance Drawable Chunk where
  draw t w chunk = drawTiles t w (tiles chunk) <> drawTiles t w (backgroundTiles chunk)
    where
      drawTiles :: Double -> World -> IOArray (Int, Int) Tile -> IO Picture
      drawTiles t w tiles = do
        assocs <- (getAssocs tiles :: IO [((Int, Int), Tile)])
        pictures <- mapM
          (\(coords, tile) ->
             draw t w tile >>= return . translate coords)
          assocs
        return $ foldr (<>) blank pictures
