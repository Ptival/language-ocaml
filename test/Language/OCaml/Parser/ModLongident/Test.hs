{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ModLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

modLongidentTests :: [String]
modLongidentTests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ModLongident" $ []
  ++ map (mkParsingTest "modLongidentP" modLongidentP) modLongidentTests

test :: IO ()
test = defaultMain unitTests
