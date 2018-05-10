{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ModLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

mod_longident_tests :: [String]
mod_longident_tests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ModLongident" $ []
  ++ map (mkParsingTest "mod_longident_P" mod_longident_P) mod_longident_tests

test :: IO ()
test = defaultMain unitTests
