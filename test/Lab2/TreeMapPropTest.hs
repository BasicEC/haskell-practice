{-# LANGUAGE TemplateHaskell #-}

module Lab2.TreeMapPropTest where

import qualified Data.Map as Map (Map, size, toList)
import Data.Maybe
import Lab2.TreeMap as TreeMap
import Test.QuickCheck

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where
  arbitrary = fmap fromMap arbitrary

fromMap :: (Ord k) => Map.Map k v -> TreeMap k v
fromMap = fromList . Map.toList

prop_ToListFromList :: (Ord k, Eq v) => TreeMap k v -> Bool
prop_ToListFromList m = m == (fromList . toList) m

prop_Concat :: (Ord k, Eq v) => TreeMap k v -> Property
prop_Concat Nil = discard
prop_Concat (Node h _ _ l r) = h > 2 ==> r1 == r2
  where
    tree1 = left l
    tree2 = right l
    tree3 = left r
    r1 = tree1 <> (tree2 <> tree3)
    r2 = (tree1 <> tree2) <> tree3

prop_Get :: (Ord k) => TreeMap k v -> k -> Bool
prop_Get m k =
  if member k m
    then isJust v
    else isNothing v
  where
    v = get k m

prop_Member :: (Ord k) => TreeMap k v -> k -> Bool
prop_Member m k = member k m == not (notMember k m)

prop_Length :: (Ord k) => Map.Map k v -> Bool
prop_Length m = length (fromMap m) == Map.size m

return []

treeMapPropTests :: IO Bool
treeMapPropTests = $quickCheckAll
