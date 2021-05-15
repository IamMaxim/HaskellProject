{-# LANGUAGE OverloadedStrings #-}

module CodeWorldRenderer where

import CodeWorld hiding (Vector)
import Control.Monad
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Vector
import Inventory
import System.Random
import World

renderRange :: Int
renderRange = 8 * 3

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
  draw t w Water =
    translated (-0.2) 0.3 (colored (RGB 0.3 (0.6 + sin (2 * t) / 20) (0.73 + sin (2 * t) / 20)) (solidRectangle 0.4 0.1))
    <> translated 0.25 (-0.2) (colored (RGB 0.3 (0.6 + sin (2 * t) / 20) (0.73 + sin (2 * t) / 20)) (solidRectangle 0.4 0.1))
    <> colored (RGB 0.16 0.49 (0.73 + sin t / 10)) (solidRectangle 1 1)

instance Drawable Entity where
  draw t w entity = (lettering . symbol) entity

instance Drawable World where
  draw t w world =
    playerInventoryShifted <> playerPicture <> entitiesPicture <> tilesPicture
    where
      assocs = Prelude.zip filteredChunkCoords (Prelude.map (\coords -> chunks world Map.! coords) filteredChunkCoords)

      centerPos = (pos . player) world
      relativePos position = position `sub` centerPos
      drawEnt ent = translate (relativePos (pos ent)) (draw t w ent)

      playerPicture = draw t w (player world)
      entitiesPicture =
        Prelude.foldMap drawEnt (entities world)

      totalInventoryWidth = fromIntegral inventorySize * inventoryCellWidth

      playerInventory = draw t w (inventory (player world))
      playerInventoryShifted = translated (- totalInventoryWidth / 2) (-9.0) playerInventory

      tilesPicture =
        Prelude.foldMap
          (\(coords, chunk) -> translate (relativePos (coords `mul` chunkSize)) (draw t w chunk))
          assocs

      filteredChunkCoords :: [Coords]
      filteredChunkCoords =
        Prelude.filter
          (not . culled)
          ((Map.keys . chunks) world)

      -- True if given coords is not in render range and should be culled
      culled :: Coords -> Bool
      culled (x, y) =
        abs (x * chunkSize - cx) > renderRange
          || abs (y * chunkSize - cy) > renderRange
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

instance (Drawable item) => Drawable (InventoryItem item) where
  draw t w inventoryItem = badge (lettering itemAmountText) <> itemPicture
    where
      itemPicture = draw t w (item inventoryItem)
      itemAmountText = pack (show (amount inventoryItem))

instance Drawable TestItem where
  draw t w (TestItem name) = colored yellow (solidRectangle 0.9 0.9)

instance (Drawable item) => Drawable (Inventory item) where
  draw t w inventory = Prelude.foldr reducer blank itemDrawings
    where
      itemDrawings =
        Data.Vector.imap
          ( \index maybeItem ->
              let ownPicture = case maybeItem of
                    Just item -> draw t w item
                    Nothing -> emptyInventoryItemPicture
                  active = index == activeItemIndex inventory
               in (inventoryCell active <> ownPicture)
          )
          (items inventory)
      reducer picture acc = picture <> translated 1 0 acc

badge :: Picture -> Picture
badge pic = translated 0.25 (-0.25) (scaled 0.3 0.3 pic)

inventoryCellHeight :: Double
inventoryCellHeight = 1

inventoryCellWidth :: Double
inventoryCellWidth = 1

emptyInventoryItemPicture :: Picture
emptyInventoryItemPicture = colored white (solidRectangle inventoryCellWidth inventoryCellHeight)

inventoryCell ::
  Bool -> -- is selected
  Picture
inventoryCell True = thickRectangle 0.05 inventoryCellWidth inventoryCellHeight
inventoryCell False = colored gray (rectangle inventoryCellWidth inventoryCellHeight)
