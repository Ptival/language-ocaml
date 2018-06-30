{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.OpenStatement.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

openStatementTests :: [String]
openStatementTests =
  [ "open A"
  , "open !A"
  -- , "(* A *) open !A"
  , "open !A (* A *)"
  -- , "(* A *) open !A (* A *)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.OpenStatement" $ []
  ++ map (mkParsingTest "openStatementP" (openStatementP structureP)) openStatementTests

test :: IO ()
test = defaultMain unitTests
