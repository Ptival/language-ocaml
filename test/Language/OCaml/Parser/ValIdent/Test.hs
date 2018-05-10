{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ValIdent.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.TestUtils

val_ident_tests :: [String]
val_ident_tests =
  [ "foo"
  , "(!)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ValIdent" $ []
  ++ map (mkParsingTest "val_ident_P" val_ident_P) val_ident_tests

test :: IO ()
test = defaultMain unitTests
