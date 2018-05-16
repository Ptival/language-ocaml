{-# language OverloadedStrings #-}

module Main where

import Test.Tasty

import qualified Language.OCaml.Parser.Common.Test
import qualified Language.OCaml.Parser.ConstrLongident.Test
import qualified Language.OCaml.Parser.Expr.Test
import qualified Language.OCaml.Parser.Implementation.Test
import qualified Language.OCaml.Parser.MatchCase.Test
import qualified Language.OCaml.Parser.ModLongident.Test
import qualified Language.OCaml.Parser.OpenStatement.Test
import qualified Language.OCaml.Parser.Pattern.Test
import qualified Language.OCaml.Parser.SeqExpr.Test
import qualified Language.OCaml.Parser.SimpleExpr.Test
import qualified Language.OCaml.Parser.SimplePattern.Test
import qualified Language.OCaml.Parser.Structure.Test
import qualified Language.OCaml.Parser.StructureItem.Test
import qualified Language.OCaml.Parser.Tokens.Test
import qualified Language.OCaml.Parser.ValIdent.Test
import qualified Language.OCaml.Parser.ValLongident.Test
import qualified Language.OCaml.PrettyPrinter.StructureItem.Test
import qualified Language.OCaml.PrettyPrinter.Structure.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $ []
  ++ [ testGroup "Parser" $
       [ Language.OCaml.Parser.Common.Test.unitTests
       , Language.OCaml.Parser.ConstrLongident.Test.unitTests
       , Language.OCaml.Parser.Expr.Test.unitTests
       , Language.OCaml.Parser.Implementation.Test.unitTests
       , Language.OCaml.Parser.MatchCase.Test.unitTests
       , Language.OCaml.Parser.ModLongident.Test.unitTests
       , Language.OCaml.Parser.OpenStatement.Test.unitTests
       , Language.OCaml.Parser.Pattern.Test.unitTests
       , Language.OCaml.Parser.SeqExpr.Test.unitTests
       , Language.OCaml.Parser.SimpleExpr.Test.unitTests
       , Language.OCaml.Parser.SimplePattern.Test.unitTests
       , Language.OCaml.Parser.Structure.Test.unitTests
       , Language.OCaml.Parser.StructureItem.Test.unitTests
       , Language.OCaml.Parser.Tokens.Test.unitTests
       , Language.OCaml.Parser.ValIdent.Test.unitTests
       , Language.OCaml.Parser.ValLongident.Test.unitTests
       ]
     ]
  ++ [ testGroup "PrettyPrinter" $
       [ Language.OCaml.PrettyPrinter.Structure.Test.unitTests
       , Language.OCaml.PrettyPrinter.StructureItem.Test.unitTests
       ]
     ]
