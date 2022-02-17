{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified ParserTests
import qualified ShareMapTests
import qualified SimplifyTests
import qualified InterpretTests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ ParserTests.tests
  , ShareMapTests.tests
  , SimplifyTests.tests
  , InterpretTests.tests
  ]
