{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.SeqExpr.Test
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
  testGroup "Language.OCaml.Parser.Generator.SeqExpr" $
    []
      ++ map (mkParsingTest parseSeqExpr) TestStrings.seqExpr

test :: IO ()
test = defaultMain unitTests
