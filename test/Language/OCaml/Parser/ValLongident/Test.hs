{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ValLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.ValLongident
import Language.OCaml.Parser.TestUtils

val_longident_tests :: [String]
val_longident_tests =
  [ "Foo.bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ValLongident" $ []
  ++ map (mkParsingTest "val_long_ident_P" val_longident_P) val_longident_tests

test :: IO ()
test = defaultMain unitTests
