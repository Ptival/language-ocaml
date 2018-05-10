{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Common.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Common
import Language.OCaml.Parser.TestUtils

constr_ident_tests :: [String]
constr_ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "[]"
  , "false"
  , "true"
  , "A "
  , "Ab "
  , "AB "
  , "[] "
  , "false "
  , "true "
  ]

ident_tests :: [String]
ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "a"
  , "aB"
  , "ab"
  , "A "
  , "Ab "
  , "AB "
  , "a "
  , "aB "
  , "ab "
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Common" $ []
  ++ map (mkParsingTest "constr_ident_P" constr_ident_P) constr_ident_tests
  ++ map (mkParsingTest "ident_P" ident_P) ident_tests

test :: IO ()
test = defaultMain unitTests
