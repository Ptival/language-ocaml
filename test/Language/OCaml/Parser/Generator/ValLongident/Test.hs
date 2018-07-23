module Language.OCaml.Parser.Generator.ValLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.TestUtils
import Language.OCaml.Parser.ValLongident.Test (testStrings)

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.ValLongident" $ []
  ++ map (mkParsingTestG parseValLongident) testStrings

test :: IO ()
test = defaultMain unitTests
