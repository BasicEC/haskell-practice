{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module Lab2Test where

import Lab2
import Test.QuickCheck
import Control.Monad

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized list' where
        list' n | n <= 0 = liftM (`Item` Nil) item
                | otherwise = liftM2 Item item list where
                    item = arbitrary
                    list = list' $ n - 1

prop_AddFirst :: Eq a => a -> List a -> Bool 
prop_AddFirst item list = let result = addFirst item list
                           in length list + 1 == length (addFirst item list)
                           && headList result == item
                           && tailList result == list

prop_DeleteItem :: Eq a => a -> List a -> Bool
prop_DeleteItem item list = case countOfItems of
                                0 -> result == list
                                1 -> notElem item result
                                     && isLengthDecreased
                                _ -> elem item result
                                     && isLengthDecreased
    where result            = deleteItem item list
          isLengthDecreased = length result == length list - 1
          countOfItems      = foldr (\x sum -> if x == item then sum + 1 else sum) 0 list

prop_ConcatNilDoesntChangeList :: Eq a => List a -> Bool
prop_ConcatNilDoesntChangeList list = Nil <> list == list && list <> Nil == list

prop_Concat :: Eq a => List a -> List a -> List a -> Bool
prop_Concat list1 list2 list3 = result1 == result2 && length result1 == length list1 + length list2 + length list3
    where result1 = list1 <> (list2 <> list3)
          result2 = (list1 <> list2) <> list3

return []
runLab2Tests :: IO Bool
runLab2Tests = $quickCheckAll
