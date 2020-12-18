module Lab2 where

import Data.Semigroup
import Data.Foldable

data List a = Nil | Item a (List a) deriving (Show,Read,Eq,Ord)

instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)
    
instance Semigroup (List a) where
   list <> Nil                 = list
   Nil <> list                 = list
   (Item value list1) <> list2 = Item value $ list1 <> list2

instance Functor List where
    fmap _ Nil           = Nil
    fmap f (Item v list) = Item (f v) $ fmap f list

instance Foldable List where
    foldr _ ini Nil              = ini
    foldr f ini (Item item list) = item `f` foldr f ini list
    foldMap f = fold . fmap f

addFirst :: a -> List a -> List a
addFirst = Item

addLast :: a -> List a -> List a
addLast item Nil               = Item item Nil
addLast item (Item value list) = Item value $ addLast item list

deleteItem :: Eq a => a -> List a -> List a
deleteItem a Nil = Nil
deleteItem a (Item x list) = if a == x then list else Item x $ deleteItem a list
