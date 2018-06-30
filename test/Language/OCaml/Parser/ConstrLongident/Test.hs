{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ConstrLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

constrLongidentTests :: [String]
constrLongidentTests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  , "[]"
  , "()"
  , "(::)"
  , "true"
  , "false"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ConstrLongident" $ []
  ++ map (mkParsingTest "constrLongidentP" constrLongidentP) constrLongidentTests

test :: IO ()
test = defaultMain unitTests
