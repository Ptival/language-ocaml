module Language.OCaml.Parser.SimpleLabeledExprList
  ( simpleLabeledExprListP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabeledSimpleExpr
import Language.OCaml.Parser.Utils.Types

-- NOTE: the OCaml version builds the list backwards for efficiency reasons
-- We call `reverse` to match it, even though it is inefficient.
-- We could update this once the whole code base is ready. #FIXME
simpleLabeledExprListP :: Parser Expression -> Parser Expression -> Parser [(ArgLabel, Expression)]
simpleLabeledExprListP seqExprP exprP = reverse <$> many (labeledSimpleExprP seqExprP exprP)
