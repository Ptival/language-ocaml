module Language.OCaml.Parser.LabeledSimpleExpr
  ( labeledSimpleExprP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleExpr
import Language.OCaml.Parser.Utils.Types

labeledSimpleExprP :: Parser Expression -> Parser Expression -> Parser (ArgLabel, Expression)
labeledSimpleExprP seqExprP exprP = choice
  [ do
    e <- simpleExprP seqExprP exprP
    return (Nolabel, e)
    -- TODO: labelExpr
  ]
