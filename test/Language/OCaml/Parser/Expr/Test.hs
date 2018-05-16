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
  , "let a = b in c"
  , "a"
  , "a, b"
  , "a, b, c"
  , "(a, b, c)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Expr" $ []
  ++ map (mkParsingTest "expr_P" expr_P) expr_tests

test :: IO ()
test = defaultMain unitTests

foo = debugParsing expr_P "a, b, c"
bar = debugParsing expr_P "(a, b, c)"
