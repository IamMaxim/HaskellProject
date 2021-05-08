
module Inventory where

data InventoryItem item  = InventoryItem {
    amount :: Int,
    item :: item
}

instance (Eq item) => Eq (InventoryItem item) where
  (==) iv1 iv2 = item iv1 == item iv2


data Inventory item = Inventory {
    activeItem :: Maybe (InventoryItem item),
    items :: [InventoryItem item]
}

defaultInventory ::
    Int -- inventory size
    -> (Int -> InventoryItem item) -- initializator for an ith item cell, will be iterated in 1..size
    -> Inventory item
defaultInventory size itemInitializer = Inventory {
    items = items,
    activeItem = headOrNothing items
}
    where
        items = map itemInitializer [1..size]

newtype TestItem = TestItem String deriving Eq

defaultTestInventory :: Inventory TestItem
defaultTestInventory = defaultInventory 10 creator
    where
        creator = \i -> InventoryItem {
            amount = i,
            item = TestItem (show i)
        }


headOrNothing :: [a] -> Maybe a
headOrNothing [] = Nothing
headOrNothing (x:xs) = Just x