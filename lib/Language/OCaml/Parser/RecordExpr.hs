module Language.OCaml.Parser.RecordExpr
  ( recordExprP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblExprList
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

recordExprP ::
  Parser Expression -> Parser Expression -> Parser (Maybe Expression, [(Loc Longident, Expression)])
recordExprP exprP simpleExprP = choice
  [ do
    e <- try $ do
      e <- simpleExprP
      withT
      return e
    l <- lblExprListP'
    return (Just e, l)
  , do
    l <- lblExprListP'
    return (Nothing, l)
  ]
  where
    lblExprListP' = lblExprListP exprP
