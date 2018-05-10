{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SimpleExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.SimpleExpr
import Language.OCaml.Parser.TestUtils

simple_expr_tests :: [String]
simple_expr_tests =
  [ "foo"
  , "foo.bar"
  , "Foo"
  , "Foo.Bar"
  , "Foo.bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.SimpleExpr" $ []
  ++ map (mkParsingTest "simple_expr_P" simple_expr_P) simple_expr_tests

test :: IO ()
test = defaultMain unitTests
