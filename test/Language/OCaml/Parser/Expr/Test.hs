{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Expr.Test
  ( test
  , testStrings
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty
-- import Text.Megaparsec

import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

testStrings :: [String]
testStrings =
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
  , "f 0 a 1 b 2 c"
  , "fun x -> x y"
  , "fun x y -> z"
  , "f a > g b"
  , "if a then b else c"
  , "if a then b"
  , "if (M.f x > 0) then a else b"
  , "if M.f x > 0 then a else b"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Expr" $ []
  ++ map (mkParsingTest "exprP" exprP) testStrings
  ++ map (compareParses "Expr" exprP parseExpr) testStrings

test :: IO ()
test = defaultMain unitTests

-- foo = debugParsing simpleLabeledExprListP "0 b c"
-- bar = debugParsing exprP "(a, b, c)"
