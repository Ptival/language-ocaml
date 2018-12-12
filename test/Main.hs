{-# language OverloadedStrings #-}

module Main where

import           Test.Tasty

-- import qualified Language.OCaml.Parser.Generator.Common.Test
-- import qualified Language.OCaml.Parser.Generator.ConstrLongident.Test
import qualified Language.OCaml.Parser.Generator.Expr.Test
import qualified Language.OCaml.Parser.Generator.Implementation.Test
import qualified Language.OCaml.Parser.Generator.LetBinding.Test
-- import qualified Language.OCaml.Parser.Generator.MatchCase.Test
-- import qualified Language.OCaml.Parser.Generator.ModLongident.Test
-- import qualified Language.OCaml.Parser.Generator.OpenStatement.Test
-- import qualified Language.OCaml.Parser.Generator.Pattern.Test
import qualified Language.OCaml.Parser.Generator.SeqExpr.Test
import qualified Language.OCaml.Parser.Generator.SimpleExpr.Test
-- import qualified Language.OCaml.Parser.Generator.SimplePattern.Test
import qualified Language.OCaml.Parser.Generator.Structure.Test
-- import qualified Language.OCaml.Parser.Generator.StructureItem.Test
-- import qualified Language.OCaml.Parser.Generator.Tokens.Test
-- import qualified Language.OCaml.Parser.Generator.ValIdent.Test
import qualified Language.OCaml.Parser.Generator.ValLongident.Test
import           Language.OCaml.Parser.Implementation.Test
-- import qualified Language.OCaml.Parser.MatchCase.Test
-- import qualified Language.OCaml.Parser.ModLongident.Test
-- import qualified Language.OCaml.Parser.OpenStatement.Test
-- import qualified Language.OCaml.Parser.Pattern.Test
-- import qualified Language.OCaml.Parser.SeqExpr.Test
-- import qualified Language.OCaml.Parser.SimpleExpr.Test
-- import qualified Language.OCaml.Parser.SimplePattern.Test
-- import qualified Language.OCaml.Parser.Structure.Test
-- import qualified Language.OCaml.Parser.StructureItem.Test
-- import qualified Language.OCaml.Parser.Tokens.Test
-- import qualified Language.OCaml.Parser.ValIdent.Test
-- import qualified Language.OCaml.Parser.ValLongident.Test
import qualified Language.OCaml.PrettyPrinter.StructureItem.Test
import qualified Language.OCaml.PrettyPrinter.Structure.Test

main :: IO ()
main = do
  mlFiles <- listMLFiles
  defaultMain $ tests mlFiles

tests :: [FilePath] -> TestTree
tests mlFiles = testGroup "Tests" $ []
  ++ [ testGroup "Parser generator" $
       -- [ Language.OCaml.Parser.Generator.Common.Test.unitTests
       -- , Language.OCaml.Parser.Generator.ConstrLongident.Test.unitTests
       [ Language.OCaml.Parser.Generator.Expr.Test.unitTests
       , Language.OCaml.Parser.Generator.LetBinding.Test.unitTests
       -- , Language.OCaml.Parser.Generator.MatchCase.Test.unitTests
       -- , Language.OCaml.Parser.Generator.ModLongident.Test.unitTests
       -- , Language.OCaml.Parser.Generator.OpenStatement.Test.unitTests
       -- , Language.OCaml.Parser.Generator.Pattern.Test.unitTests
       , Language.OCaml.Parser.Generator.SeqExpr.Test.unitTests
       , Language.OCaml.Parser.Generator.SimpleExpr.Test.unitTests
       -- , Language.OCaml.Parser.Generator.SimplePattern.Test.unitTests
       , Language.OCaml.Parser.Generator.Structure.Test.unitTests
       -- , Language.OCaml.Parser.Generator.StructureItem.Test.unitTests
       -- , Language.OCaml.Parser.Generator.Tokens.Test.unitTests
       -- , Language.OCaml.Parser.Generator.ValIdent.Test.unitTests
       , Language.OCaml.Parser.Generator.ValLongident.Test.unitTests
       ]
       ++ [ Language.OCaml.Parser.Generator.Implementation.Test.mkUnitTest mlFiles ]
     ]
  ++ [ testGroup "PrettyPrinter" $
       [ Language.OCaml.PrettyPrinter.Structure.Test.unitTests
       , Language.OCaml.PrettyPrinter.StructureItem.Test.unitTests
       ]
     ]
