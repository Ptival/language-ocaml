{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.SimpleExpr
  ( simple_expr_P
  ) where

import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Constant
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.LabelLongident
import Language.OCaml.Parser.ValLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators

simple_expr_P :: Parser Expression
simple_expr_P = leftRecursive
  [ do
    i <- val_longident_P
    return . mkexp $ Pexp_ident (mkRHS i 1)
  , mkexp . Pexp_constant <$> constant_P
  , do
    i <- constr_longident_P
    return $ mkexp $ Pexp_construct (mkRHS i 1) Nothing
  ]
  [ do
    dot_T
    i <- label_longident_P
    return $ \ x -> mkexp $ Pexp_field x (mkRHS i 3)
  ]
