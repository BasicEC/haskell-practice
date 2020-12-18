module Lab2Test
    ( lab2TestList
    ) where

import Lab2
import Test.HUnit

addFirstCorrect = TestList 
    [ TestCase (assertEqual "addFirst to Nil" (Item 1 Nil) (addFirst 1 Nil))
    , TestCase (let list     = Item 2 (Item 3 Nil)
                    expected = Item 1 list
                in assertEqual "addFirst to list" (addFirst 1 list) expected
               )
    ]

addLastCorrect = TestList 
    [ TestCase (assertEqual "addLast to Nil" (Item 1 Nil) (addLast 1 Nil))
    , TestCase (let list     = Item 3 (Item 2 Nil)
                    expected = (Item 3 (Item 2 (Item 1 Nil)))
                in assertEqual "addLast to list" (addLast 1 list) expected
               )
    ]

deleteItemCorrect = TestList 
    [ TestCase (assertEqual "deleteItem from Nil" Nil (deleteItem 1 Nil))
    , TestCase (let list     = Item 2 (Item 3 Nil)
                    expected = list
                in assertEqual "deleteItem Nonexistent item" (deleteItem 1 list) expected
               )
    , TestCase (let list     = Item 2 (Item 3 (Item 2 (Item 3 Nil)))
                    expected = Item 2 (Item 2 (Item 3 Nil))
                in assertEqual "deleteItem only first item" (deleteItem 3 list) expected
               )
    ]

lab2TestList = TestList 
    [ deleteItemCorrect
    , addFirstCorrect
    , addLastCorrect
    ]
