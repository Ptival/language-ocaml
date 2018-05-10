{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SimplePattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

simple_pattern_tests :: [String]
simple_pattern_tests =
  [ "_"
  , "a"
  , "A"
  , "A.B"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Pattern" $ []
  ++ map (mkParsingTest "simple_pattern_P" simple_pattern_P) simple_pattern_tests

test :: IO ()
test = defaultMain unitTests
