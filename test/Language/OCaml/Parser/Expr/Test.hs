{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Expr.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

expr_tests :: [String]
expr_tests =
  [ "Foo.Bar"
  , "function a -> b"
  , "function a -> Foo.bar_baz"
  , "function Foo _ -> b"
  , [s|function Foo _ -> "some_stuff"|]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Expr" $ []
  ++ map (mkParsingTest "expr_P" (expr_P seq_expr_P)) expr_tests

test :: IO ()
test = defaultMain unitTests
