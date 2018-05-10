{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.MatchCase.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

match_case_tests :: [String]
match_case_tests =
  [ "a -> b"
  , "a -> B.c_d"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.MatchCase" $ []
  ++ map (mkParsingTest "match_case_P" (match_case_P seq_expr_P)) match_case_tests

test :: IO ()
test = defaultMain unitTests
