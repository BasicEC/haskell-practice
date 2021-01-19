module Lab2.TreeMapTest where

import Lab2.TreeMap as TreeMap
import Test.HUnit

deleteFromNil :: Test
deleteFromNil = "delete from Nil" ~: empty @=? delete 1 empty
  where
    empty = Nil :: TreeMap Integer Integer


treeMapTests :: Test
treeMapTests =
  TestList
    [ deleteFromNil
    ]