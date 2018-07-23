{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.ModLongident.Test
  ( test
  , testStrings
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

testStrings :: [String]
testStrings =
  [ "Mod"
  , "Mod.Mod"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.ModLongident" $ []
  ++ map (mkParsingTest "modLongidentP" modLongidentP) testStrings

test :: IO ()
test = defaultMain unitTests
