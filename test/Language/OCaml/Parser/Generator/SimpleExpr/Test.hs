module Language.OCaml.Parser.Generator.SimpleExpr.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty

import           Language.OCaml.Parser.Generator.Parser
import qualified Language.OCaml.Parser.TestStrings as TestStrings
import           Language.OCaml.Parser.TestUtils

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.SimpleExpr" $ []
  ++ map (mkParsingTest parseSimpleExpr) TestStrings.simpleExpr

test :: IO ()
test = defaultMain unitTests
