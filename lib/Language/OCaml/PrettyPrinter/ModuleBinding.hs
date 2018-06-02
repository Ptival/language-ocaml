{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleBinding
  ( module_binding_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleExpr ()

module_binding_PP :: Module_binding -> Doc a
module_binding_PP d = fillSep [ "module", name, "=", expr ]
  where
    name = pretty $ pmb_name d
    expr = pretty $ pmb_expr d

instance Pretty Module_binding where
  pretty = module_binding_PP
