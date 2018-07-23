{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.Constant.Test
  ( test
  , testStrings
  , unitTests
  ) where

import           Test.Tasty

import           Language.OCaml.Parser.Internal
import qualified Language.OCaml.Parser.Int.Test as Int
import           Language.OCaml.Parser.TestUtils

testStrings :: [String]
testStrings = []
  ++ Int.testStrings
  -- TODO: CHAR
  -- TODO: STRING
  -- TODO: FLOAT

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Constant" $ []
  ++ map (mkParsingTest "valLongIdentP" valLongidentP) testStrings

test :: IO ()
test = defaultMain unitTests
