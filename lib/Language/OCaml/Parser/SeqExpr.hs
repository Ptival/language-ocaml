module Language.OCaml.Parser.SeqExpr
  ( seqExprP
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Expr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

seqExprP :: Parser Structure -> Parser Expression
seqExprP structureP = choice
  [ try $ do
    e <- exprP'
    semiT
    s <- seqExprP'
    return $ mkExp def (PexpSequence e s)
  , try $ do
    e <- exprP'
    semiT
    return e
  , try $ exprP'
  -- TODO: percent
  ]
  where
    exprP' = exprP structureP seqExprP'
    seqExprP' = seqExprP structureP
