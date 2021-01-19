module Main where

import Lab2.TreeMapPropTest
import Lab2.TreeMapTest
import Lab2.ListPropTest
import Lab2.ListTest
import Test.HUnit (runTestTT)

main :: IO Bool
main = do
  putStrLn "\n=== List Tests ==="
  runTestTT listTests
  listPropTests
  putStrLn "=== TreeMap Tests ==="
  runTestTT treeMapTests
  treeMapPropTests
