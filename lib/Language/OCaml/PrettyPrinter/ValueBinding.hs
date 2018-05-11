{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ValueBinding
  ( value_binding_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Expression ()
import Language.OCaml.PrettyPrinter.Pattern ()

value_binding_PP :: Value_binding -> Doc a
value_binding_PP d = fillSep [ pattern', "=", expr ]
  where
    pattern' = pretty $ pvb_pat d
    expr     = pretty $ pvb_expr d

instance Pretty Value_binding where
  pretty = value_binding_PP
