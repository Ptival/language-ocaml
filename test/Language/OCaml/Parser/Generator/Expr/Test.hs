module Language.OCaml.Parser.Generator.Expr.Test
  ( test,
    unitTests,
  )
where

import Language.OCaml.Parser.Generator.Parser
import qualified Language.OCaml.Parser.TestStrings as TestStrings
import Language.OCaml.Parser.TestUtils
import Test.Tasty

unitTests :: TestTree
unitTests =
  testGroup "Language.OCaml.Parser.Generator.Expr" $
    []
      ++ map (mkParsingTest parseExpr) TestStrings.expr

test :: IO ()
test = defaultMain unitTests
