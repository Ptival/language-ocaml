{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.Pattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

patternTests :: [String]
patternTests =
  [ "_"
  , "a"
  , "A"
  , "A a"
  , "A B"
  , "A _"
  , "A _ | B _"
  , "_, _"
  , "A _, B _"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Pattern" $ []
  ++ map (mkParsingTest "patternP" patternP) patternTests

test :: IO ()
test = defaultMain unitTests
