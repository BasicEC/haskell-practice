module Lab2.List where

import Data.Foldable ()
import Data.Semigroup ()

data List a = Nil | Item a (List a) deriving (Show, Read, Eq, Ord)

instance Monoid (List a) where
  mempty  = Nil
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

addFirst :: a -> List a -> List a
addFirst = Item

deleteItem :: Eq a => a -> List a -> List a
deleteItem _ Nil           = Nil
deleteItem a (Item x list) = if a == x then list else Item x $ deleteItem a list

headList :: List a -> a
headList Nil           = error "empty list"
headList (Item item _) = item

tailList :: List a -> List a
tailList Nil = error "empty list"
tailList (Item _ list) = list
