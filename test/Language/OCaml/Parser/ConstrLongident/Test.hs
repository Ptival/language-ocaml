{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ConstrLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.TestUtils

constr_longident_tests :: [String]
constr_longident_tests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ConstrLongident" $ []
  ++ map (mkParsingTest "constr_longident_P" constr_longident_P) constr_longident_tests

test :: IO ()
test = defaultMain unitTests
