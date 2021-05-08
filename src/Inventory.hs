{-# LANGUAGE StandaloneDeriving #-}
module Inventory where

import Data.Vector

data InventoryItem item  = InventoryItem {
    amount :: Int,
    item :: item
}

instance (Eq item) => Eq (InventoryItem item) where
  (==) iv1 iv2 = item iv1 == item iv2

deriving instance (Show item) => Show (InventoryItem item)  

data Inventory item = Inventory {
    activeItemIndex :: Int,
    items :: Vector (Maybe (InventoryItem item))
} deriving Show

inventorySize :: Int
inventorySize = 9

emptyInventory :: Inventory item
emptyInventory = createInventory (const Nothing)

createInventory ::
    (Int -> Maybe (InventoryItem item)) -- initializator for an ith item cell, will be iterated in 1..size
    -> Inventory item
createInventory itemInitializer = Inventory {
    items = items,
    activeItemIndex = 0
}
    where
        items = generate inventorySize itemInitializer

newtype TestItem = TestItem String deriving (Eq, Show)

createTestInventory :: Inventory TestItem
createTestInventory = createInventory creator
    where
        creator = \i -> case () of
                _ | even i -> Just InventoryItem {
                    amount = i,
                    item = TestItem (show i)
                }
                    | otherwise -> Nothing

changeActiveItem ::
    Inventory item -- current inventory
    -> Int -- new active index
    -> Inventory item -- new inventory
changeActiveItem inventory newActiveIndex
    | newActiveIndex < 0 || newActiveIndex > inventorySize = inventory
    | otherwise = inventory { activeItemIndex = newActiveIndex }

addToInventoryItem :: InventoryItem item -> InventoryItem item
addToInventoryItem item = item { amount = amount item + 1 }

addItem :: (Eq item) =>
     Inventory item -- current invenory
     -> item -- new item
     ->  Inventory item
addItem inventory newItem = inventory { items = updatedItems }
    where
        inventoryItems = items inventory
        currentItems = Data.Vector.map (fmap item) inventoryItems

        itemIndex = Data.Vector.elemIndex (Just newItem) currentItems
        firstEmptyIndex = Data.Vector.findIndex (== Nothing) currentItems

        updatedItems = case itemIndex of
            Just index -> updatedInventoryItems
                where
                    existingItemMaybe = inventoryItems !? index

                    updatedInventoryItems = case existingItemMaybe of
                        Just (Just existingItem) -> inventoryItems // [(index, Just (addToInventoryItem existingItem))]
                        _ -> inventoryItems
            Nothing -> case firstEmptyIndex of
                Just emptyIndex -> inventoryItems // [(emptyIndex, Just newInventoryItem)]
                    where 
                        newInventoryItem = InventoryItem {
                            amount = 1,
                            item = newItem
                        }
                Nothing -> inventoryItems -- Maybe do something if insertion falied