{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.Pattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.TestUtils

pattern_tests :: [String]
pattern_tests =
  [ "_"
  , "a"
  , "A"
  , "A a"
  , "A B"
  , "A _"
  , "A _ | B _"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Pattern" $ []
  ++ map (mkParsingTest "pattern_P" pattern_P) pattern_tests

test :: IO ()
test = defaultMain unitTests
