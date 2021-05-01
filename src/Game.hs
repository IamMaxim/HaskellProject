

module Game where

import World
import CodeWorld
import WorldGen
import CodeWorldRenderer


gameMain :: IO ()
gameMain = do
  world <- genWorld
  drawing <- draw 0 world world
  drawingOf drawing


-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
--
-- -- I splitted the code across several sections of the file to make navigating
-- -- and reading easier. Also, I ordered the code in a top-down approach, so the
-- -- top-level function of each section is on top of the section. Again, for
-- -- easier reading and navigation.
--
-- module Game (gameMain) where
--
-- import CodeWorld
-- import Data.Maybe
--
-- -----------
-- -- STATE --
-- -----------
--
-- -- All doors on the map have a specific color, which is used to toggle them
-- -- with appropriate buttons. This is the type for that.
-- -- It may seem a bit stupid to redefine a "color" type again when we have
-- -- CodeWorld one, but I want more flexibility. Maybe in future it won't be a
-- -- simple color.
-- --
-- -- Sorry, I'm not sure if it is good to use Eq class before we covered classes
-- -- during the classes, but I found this the most elegant solution in this case.
-- data DoorColor = Red | Green | Purple | Orange deriving (Eq)
--
-- -- All tiles of the map are of this type.
-- data Tile = Grass | Floor | Wall | Button DoorColor | Door DoorColor | Exit | Void
--
-- -- Type alias for convenient representation of 2D world tiles.
-- -- In the future, this may be replaced by the Array to improve random access
-- -- performance (and random access is used to access the elements of the world
-- -- tiles).
-- type Tiles = [[Tile]]
--
-- -- A shorthand for a vector of 2 Ints.
-- type Coords = (Int, Int)
--
-- -- Possible directions the player may move in.
-- data Dir = Left | Right | Up | Down
--
-- -- State stores all the required info about the level, i.e. world itself, state
-- -- or interactables, player state, etc.
-- --
-- -- I'd like to make the world fully mutable, so maybe in future we could alter
-- -- the things. Concequently, the entire level is stored as a 2D matrix of
-- -- tiles.
-- data State = State
--   { tiles :: Tiles,
--     playerPos :: Coords,
--     hasWon :: Bool,
--     showStartScreen :: Bool
--   }
--
-- -----------------------
-- -- WORLD INTERACTION --
-- -----------------------
--
-- -- Conversion from Dir data type to a vector to allow general case movement.
-- getDirVector :: Dir -> Coords
-- getDirVector Game.Left = (-1, 0)
-- getDirVector Game.Right = (1, 0)
-- getDirVector Game.Up = (0, 1)
-- getDirVector Game.Down = (0, -1)
--
-- -- Processes user input and updates the world state.
-- processInput :: Event -> State -> State
-- processInput event state
--   -- Handle start screen
--   | showStartScreen state = case event of
--     KeyPress key
--       | key == " " -> state {showStartScreen = False}
--       | otherwise -> state
--     _any -> state
--   -- Handle finish screen
--   | hasWon state = state
--   -- Handle the game itself
--   | otherwise = case event of
--     KeyPress key
--       | key == "W" -> updateWorld Game.Up state
--       | key == "A" -> updateWorld Game.Left state
--       | key == "S" -> updateWorld Game.Down state
--       | key == "D" -> updateWorld Game.Right state
--       | key == "Esc" -> initialState
--     _any -> state
--
-- -- Updates the state by trying to move in a given direction.
-- -- Then, ticks the world.
-- updateWorld :: Dir -> State -> State
-- updateWorld dir state = tick (state {playerPos = tryMove dir state})
--
-- -- Try move character in a given direction.
-- -- "Game exploiters" may notice that our world is not actually bound by
-- -- something other than walls. So, theoretically, with right map, we can escape
-- -- the level boudaries and get into the "void".
-- tryMove :: Dir -> State -> Coords
-- tryMove dir state =
--   if canMove (tileAt newCoords state)
--     then newCoords
--     else (x, y)
--   where
--     (x, y) = playerPos state
--     (dirX, dirY) = getDirVector dir
--     newCoords = (x + dirX, y + dirY)
--
-- -- Tells if it is possible to step on a given tile type.
-- canMove :: Tile -> Bool
-- canMove Wall = False
-- canMove (Door _) = False
-- canMove _ = True
--
-- -- Safely gets a tile of the map. If no value exists at given coords, Void is
-- -- returned.
-- tileAt :: Coords -> State -> Tile
-- tileAt (x, y) state
--   | isNothing row = Void
--   | otherwise = fromMaybe Void elem
--   where
--     row = tiles state `safeElementAt` y
--     elem = fromJust row `safeElementAt` x
--
-- -- Processes the game logic and returns the updated state.
-- tick :: State -> State
-- tick state = case playerTile of
--   Button c -> openDoors state c
--   Exit -> state {hasWon = True}
--   _ -> state
--   where
--     playerTile = tileAt (playerPos state) state
--
-- -- Replaces all doors and buttons of given color with floor tile.
-- openDoors :: State -> DoorColor -> State
-- openDoors state color =
--   state
--     { tiles =
--         map
--           ( map
--               ( \x -> case x of
--                   Button dc -> if color == dc then Floor else Button dc
--                   Door dc -> if color == dc then Floor else Door dc
--                   _otherwise -> x
--               )
--           )
--           (tiles state)
--     }
--
-- ---------------
-- -- RENDERING --
-- ---------------
--
-- renderWorld :: State -> Picture
-- renderWorld state
--   | showStartScreen state =
--     colored
--       white
--       ( translated 0 (-5) (lettering "Press SPACE to start")
--           <> lettering "An amazing tile game"
--       )
--       <> solidRectangle 100 100
--   | otherwise =
--     ( if hasWon state
--         then -- Render text if player has won
--           colored white (lettering "Ну привет") <> colored (RGB 0.3 0.3 0.3) (solidRectangle 6 2)
--         else blank
--     )
--       -- player
--       <> lettering "🚶"
--       -- world
--       <> translated
--         offsetX
--         offsetY
--         ( -- world tiles
--           renderTiles 0 (tiles state)
--             -- gray background
--             <> colored (RGB 0.2 0.2 0.2) (solidRectangle 100 100)
--         )
--   where
--     (px, py) = playerPos state
--     (offsetX, offsetY) = (fromIntegral (- px), fromIntegral (- py))
--
-- -- Renders tiles of the entire level map.
-- renderTiles :: Int -> [[Tile]] -> Picture
-- renderTiles _y [] = blank
-- renderTiles y (row : rows) =
--   renderTilesRow 0 y row <> renderTiles (y + 1) rows
--   where
--     renderTilesRow :: Int -> Int -> [Tile] -> Picture
--     renderTilesRow _x _y [] = blank
--     renderTilesRow x y (tile : tiles) =
--       translated (fromIntegral x) (fromIntegral y) (renderTile tile)
--         <> renderTilesRow (x + 1) y tiles
--
-- -- Renders a single tile.
-- renderTile :: Tile -> Picture
-- renderTile Void = colored (RGB 0.2 0.2 0.2) (solidRectangle 1 1)
-- renderTile Grass = colored (RGB 0.29 0.69 0.39) (solidRectangle 1 1)
-- renderTile Floor = colored yellow (solidRectangle 1 1)
-- renderTile Wall = colored black (solidRectangle 1 1)
-- renderTile (Button color) = colored (doorColorOf color) (solidCircle 0.25)
-- renderTile (Door color) = colored (doorColorOf color) (solidRectangle 0.8 0.8)
-- renderTile Exit = colored (RGB 0 0.8 1) (solidRectangle 1 1)
--
-- -- Returns the particular RGB color for a DoorColor.
-- doorColorOf :: DoorColor -> Color
-- doorColorOf Red = red
-- doorColorOf Green = green
-- doorColorOf Purple = purple
-- doorColorOf Orange = orange
--
-- ---------------------------
-- -- INSTANCE OF THE LEVEL --
-- ---------------------------
--
-- -- Our level has a size of 21x21 tiles. Player starts at (11, 11).
-- initialState :: State
-- initialState =
--   State
--     { tiles = genLevel 21 21,
--       playerPos = (10, 10),
--       hasWon = False,
--       showStartScreen = True
--     }
--
-- -- A shorthand for generating a level.
-- genLevel :: Int -> Int -> Tiles
-- genLevel xSize ySize = genTiles (0, xSize) (0, ySize)
--
-- -- Generates a matrix of tiles at given x and y ranges.
-- genTiles :: (Int, Int) -> (Int, Int) -> [[Tile]]
-- genTiles xRange (fromY, toY)
--   | fromY >= toY = []
--   | otherwise = genTileRow xRange fromY : genTiles xRange (fromY + 1, toY)
--   where
--     genTileRow :: (Int, Int) -> Int -> [Tile]
--     genTileRow (fromX, toX) y
--       | fromX >= toX = []
--       | otherwise = genTileAt (fromX, y) : genTileRow (fromX + 1, toX) y
--
-- -- Sample the specific tile for an initial world state.
-- genTileAt :: Coords -> Tile
-- genTileAt (x, y)
--   -- Hole to the void
--   | x == 1 && y == 20 = Floor
--   -- Exit
--   | x == 18 && y == 1 = Exit
--   -- Bottom toggles
--   | x == 2 && y == 1 = Button Purple
--   | x == 6 && y == 2 = Button Green
--   | x == 10 && y == 1 = Button Red
--   | x == 14 && y == 2 = Button Orange
--   -- Bottom door rows
--   | isDoorRow (6, 3) (x, y) = Door Purple
--   | isDoorRow (10, 0) (x, y) = Door Green
--   | isDoorRow (14, 3) (x, y) = Door Red
--   | isDoorRow (18, 0) (x, y) = Door Orange
--   -- Bottom spaces
--   | isHollowBox (2, 1) (x, y) = Floor
--   | isHollowBox (6, 2) (x, y) = Floor
--   | isHollowBox (10, 1) (x, y) = Floor
--   | isHollowBox (14, 2) (x, y) = Floor
--   | isHollowBox (18, 1) (x, y) = Floor
--   -- Bottom toggles
--   -- Wall surrounding the play area
--   | x == 0 || y <= 3 || x == 20 || y == 20 = Wall
--   | otherwise = Grass
--
-- isDoorRow :: Coords -> Coords -> Bool
-- isDoorRow (rowX, rowY) (x, y) =
--   x >= rowX - 1 && x <= rowX + 1 && rowY == y
--
-- isHollowBox :: Coords -> Coords -> Bool
-- isHollowBox (boxX, boxY) (x, y) =
--   x >= boxX - 1 && x <= boxX + 1
--     && y >= boxY - 1
--     && y <= boxY + 1
--
-- -----------
-- -- UTILS --
-- -----------
--
-- -- Function to fix that strange paradox with strictly-typed pure language that
-- -- may throw an error if index of element in the list is out of range.
-- safeElementAt :: [a] -> Int -> Maybe a
-- safeElementAt l i =
--   if i < 0 || i >= length l
--     then Nothing
--     else Just (l !! i)
--
-- ----------
-- -- MAIN --
-- ----------
--
-- gameMain :: IO ()
-- gameMain = activityOf initialState processInput renderWorld
--
