{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.SeqExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

seqExprTests :: [String]
seqExprTests =
  [ "b"
  , "Foo.bar"
  , "Foo.bar_baz"
  , "M.f x > 0"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.SeqExpr" $ []
  ++ map (mkParsingTest "seqExprP" seqExprP) seqExprTests

test :: IO ()
test = defaultMain unitTests
