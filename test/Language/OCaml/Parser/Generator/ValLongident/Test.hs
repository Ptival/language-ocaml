module Language.OCaml.Parser.Generator.ValLongident.Test
  ( test,
    unitTests,
  )
where

import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.TestUtils
import Language.OCaml.Parser.ValLongident.Test (testStrings)
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup "Language.OCaml.Parser.Generator.ValLongident" $
    []
      ++ map (mkParsingTest parseValLongident) testStrings

test :: IO ()
test = defaultMain unitTests
