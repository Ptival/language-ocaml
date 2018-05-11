{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleExprDesc
  ( module_expr_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

module_expr_desc_PP :: Module_expr_desc -> Doc a
module_expr_desc_PP = \case
  Pmod_ident i -> pretty i

instance Pretty Module_expr_desc where
  pretty = module_expr_desc_PP
