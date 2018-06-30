{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SimpleExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

simpleExprTests :: [String]
simpleExprTests =
  [ "foo"
  , "foo.bar"
  , "Foo"
  , "Foo.Bar"
  , "Foo.bar"
  , "(foo)"
  , "(foo, bar)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.SimpleExpr" $ []
  ++ map (mkParsingTest "simpleExprP" simpleExprP) simpleExprTests

test :: IO ()
test = defaultMain unitTests

-- debug = debugParsing simpleExprP "(foo)"
