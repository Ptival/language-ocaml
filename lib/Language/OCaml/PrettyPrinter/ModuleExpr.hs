{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleExpr
  ( module_expr_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleExprDesc ()

module_expr_PP :: Module_expr -> Doc a
module_expr_PP = pretty . pmod_desc

instance Pretty Module_expr where
  pretty = module_expr_PP
