module Main where

import Lab2Test
import Test.HUnit

main :: IO Counts
main = do
    runTestTT $ TestList
        [
          lab2TestList
        ]