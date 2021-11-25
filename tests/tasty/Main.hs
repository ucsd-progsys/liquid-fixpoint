{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified ParserTests
import qualified ShareMapTests
import qualified SimplifiableTests
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ ParserTests.tests
  , ShareMapTests.tests
  , SimplifiableTests.tests
  ]
