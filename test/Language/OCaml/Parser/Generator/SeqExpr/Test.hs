{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.SeqExpr.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty

import           Language.OCaml.Parser.Generator.Parser
import           Language.OCaml.Parser.TestUtils
import qualified Language.OCaml.Parser.TestStrings      as TestStrings

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.SeqExpr" $ []
  ++ map (mkParsingTest parseSeqExpr) TestStrings.seqExpr

test :: IO ()
test = defaultMain unitTests
