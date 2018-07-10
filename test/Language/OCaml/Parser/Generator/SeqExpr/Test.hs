{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.SeqExpr.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

-- import Language.OCaml.Parser.Generator.Lexer
import Language.OCaml.Parser.Generator.Parser
-- import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils
import Language.OCaml.Parser.SeqExpr.Test (testStrings)

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.SeqExpr" $ []
  ++ map (mkParsingTestG "parseSeqExpr" parseSeqExpr) testStrings
  -- ++ map (compareParses "SeqExpr" seqExprP parseSeqExpr) testStrings

test :: IO ()
test = defaultMain unitTests
