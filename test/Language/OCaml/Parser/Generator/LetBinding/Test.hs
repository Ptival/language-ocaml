module Language.OCaml.Parser.Generator.LetBinding.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty

import           Language.OCaml.Parser.Generator.Parser
import           Language.OCaml.Parser.TestUtils
import qualified Language.OCaml.Parser.TestStrings as TestStrings

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Structure" $ []
  ++ map (mkParsingTest parseLetBinding) TestStrings.letBinding

test :: IO ()
test = defaultMain unitTests
