{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SeqExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.TestUtils

seq_expr_tests :: [String]
seq_expr_tests =
  [ "b"
  , "Foo.bar"
  , "Foo.bar_baz"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.SeqExpr" $ []
  ++ map (mkParsingTest "seq_expr_P" seq_expr_P) seq_expr_tests

test :: IO ()
test = defaultMain unitTests
