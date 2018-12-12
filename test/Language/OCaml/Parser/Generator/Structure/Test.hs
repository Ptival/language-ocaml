module Language.OCaml.Parser.Generator.Structure.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty

import           Language.OCaml.Parser.Generator.Parser
import           Language.OCaml.Parser.TestUtils
import qualified Language.OCaml.Parser.TestStrings as TestStrings

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Structure" $ []
  ++ map (mkParsingTest parseStructure) TestStrings.structure

test :: IO ()
test = defaultMain unitTests
