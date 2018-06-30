{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ValIdent.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

valIdentTests :: [String]
valIdentTests =
  [ "foo"
  , "(!)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ValIdent" $ []
  ++ map (mkParsingTest "valIdentP" valIdentP) valIdentTests

test :: IO ()
test = defaultMain unitTests
