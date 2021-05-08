
module Inventory where

data InventoryItem item  = InventoryItem {
    amount :: Int,
    item :: item
}

instance (Eq item) => Eq (InventoryItem item) where
  (==) iv1 iv2 = item iv1 == item iv2


data Inventory item = Inventory {
    activeItemIndex :: Int,
    items :: [Maybe (InventoryItem item)]
}

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
        items = map itemInitializer [1..inventorySize]

newtype TestItem = TestItem String deriving Eq

createTestInventory :: Inventory TestItem
createTestInventory = createInventory creator
    where
        creator = \i -> case () of
                _ | even i -> Just InventoryItem {
                    amount = i,
                    item = TestItem (show i)
                }
                    | otherwise -> Nothing