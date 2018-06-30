{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SimplePattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

simplePatternTests :: [String]
simplePatternTests =
  [ "_"
  , "a"
  , "A"
  , "A.B"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Pattern" $ []
  ++ map (mkParsingTest "simplePatternP" simplePatternP) simplePatternTests

test :: IO ()
test = defaultMain unitTests
