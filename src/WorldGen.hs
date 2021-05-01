{-# LANGUAGE OverloadedStrings #-}

-- | This module is responsible for creating/generating the world.
module WorldGen where

import Data.Array
import World

-- | Generates the initial world state
genWorld :: World
genWorld =
  World
    { chunks = _,
      player =
        Entity
          { pos = (0, 0),
            name = "Player",
            symbol = "P"
          },
      entities = []
    }

genChunk :: Coords -> Chunk
genChunk _ = _
--   chunk
--     { tiles = array ((0, 0) (15, 15)) []
--     }

-- | Generates additional chunks as needed if there are not enough of them in the player range
updateChunks :: Coords -> World -> World
updateChunks coords world = _
  where
    -- gen list of chunk positions (with step of 16)
    chunksToCheck = _
