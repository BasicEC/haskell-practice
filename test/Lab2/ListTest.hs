module Lab2.ListTest
  ( listTests,
  )
where

import Lab2.List
import Test.HUnit
import Prelude hiding (tail)

deleteFromNil :: Test
deleteFromNil = "delete from Nil" ~: Nil @=? delete 1 Nil

listTests :: Test
listTests =
  TestList
    [ deleteFromNil
    ]
