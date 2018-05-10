{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.OpenStatement.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.OpenStatement
import Language.OCaml.Parser.Structure
import Language.OCaml.Parser.TestUtils

open_statement_tests :: [String]
open_statement_tests =
  [ "open A"
  , "open !A"
  -- , "(* A *) open !A"
  , "open !A (* A *)"
  -- , "(* A *) open !A (* A *)"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.OpenStatement" $ []
  ++ map (mkParsingTest "open_statement_P" (open_statement_P structure_P)) open_statement_tests

test :: IO ()
test = defaultMain unitTests
