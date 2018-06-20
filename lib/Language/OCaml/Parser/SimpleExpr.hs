module Language.OCaml.Parser.SimpleExpr
  ( simple_expr_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Constant
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.ExprSemiList
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.OptSemi
import Language.OCaml.Parser.RecordExpr
import Language.OCaml.Parser.ValLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

simple_expr_P :: Parser Expression -> Parser Expression -> Parser Expression
simple_expr_P seq_expr_P expr_P = leftRecursive
  [ do
    i <- val_longident_P
    return . mkexp $ Pexp_ident (mkRHS i 1)
  , mkexp . Pexp_constant <$> constant_P
  , do
    i <- constr_longident_P
    return $ mkexp $ Pexp_construct (mkRHS i 1) Nothing
  , do
    l_paren_T
    e <- seq_expr_P
    r_paren_T
    return $ reloc_exp e
  , do
    l_bracket_T
    l <- expr_semi_list_P expr_P
    opt_semi_P
    r_bracket_T
    return $ reloc_exp $ mktailexp (rhsLoc 4) (reverse l)
  , do
    l_brace_T
    (exten, fields) <- record_expr_P expr_P simple_expr_P'
    r_brace_T
    return $ mkexp $ Pexp_record fields exten
  ]
  [ do
    dot_T
    i <- label_longident_P
    return $ \ x -> mkexp $ Pexp_field x (mkRHS i 3)
  ]
  where
    simple_expr_P' = simple_expr_P seq_expr_P expr_P
