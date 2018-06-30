{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ValLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

valLongidentTests :: [String]
valLongidentTests =
  [ "Foo.bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ValLongident" $ []
  ++ map (mkParsingTest "valLongIdentP" valLongidentP) valLongidentTests

test :: IO ()
test = defaultMain unitTests
