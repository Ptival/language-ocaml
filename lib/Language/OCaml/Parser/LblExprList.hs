module Language.OCaml.Parser.LblExprList
  ( lblExprListP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LblExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

-- FIXME: Right now, this is made to look like the OCaml parser, but we could
-- optimize everything by factoring out the left parsers that are the same.

lblExprListP :: Parser Expression -> Parser [(Loc Longident, Expression)]
lblExprListP exprP = choice
  [ try $ do
    e <- lblExprP'
    semiT
    l <- lblExprListP'
    return $ e : l
  , try $ do
    e <- lblExprP'
    semiT
    return [e]
  , (: []) <$> lblExprP'
  ]
  where
    lblExprP'      = lblExprP      exprP
    lblExprListP' = lblExprListP exprP
