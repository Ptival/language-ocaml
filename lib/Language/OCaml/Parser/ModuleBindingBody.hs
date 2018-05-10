{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ModuleBindingBody
  ( module_binding_body_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModuleExpr
import Language.OCaml.Parser.Tokens

module_binding_body_P :: Parser Module_expr
module_binding_body_P = choice
  [ do
    try $ equal_T
    module_expr_P
    -- TODO: rest
  ]
