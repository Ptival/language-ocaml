{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.Expr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.TestUtils
import Language.OCaml.Parser.Expr.Test (testStrings)

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Expr" $ []
  ++ map (mkParsingTestG "Expr" parseExpr) testStrings

test :: IO ()
test = defaultMain unitTests
