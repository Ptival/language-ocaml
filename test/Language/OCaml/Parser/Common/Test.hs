{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Common.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

constrIdentTests :: [String]
constrIdentTests =
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

identTests :: [String]
identTests =
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
  ++ map (mkParsingTest "constrIdentP" constrIdentP) constrIdentTests
  ++ map (mkParsingTest "identP" identP) identTests

test :: IO ()
test = defaultMain unitTests
