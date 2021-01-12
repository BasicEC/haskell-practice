{-# LANGUAGE TemplateHaskell #-}

module Lab2.TreeMapPropTest where

import Lab2.TreeMap as TreeMap
import Data.Map as Map (toList)
import Test.QuickCheck
import Data.Maybe

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where
  arbitrary = fmap (TreeMap.fromList . Map.toList) arbitrary

prop_ToListFromList :: (Ord k, Eq v) => TreeMap k v -> Bool
prop_ToListFromList m = m == (TreeMap.fromList . TreeMap.toList) m

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
    where v = get k m

prop_Member :: (Ord k) => TreeMap k v -> k -> Bool
prop_Member m k = member k m == not (notMember k m)

return []
runLab2Tests :: IO Bool
runLab2Tests = $quickCheckAll
