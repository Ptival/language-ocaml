{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.ValueBinding
  ( value_binding_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Pattern ()

value_binding_PP :: (Pretty Expression) => Value_binding -> Doc a
value_binding_PP d = fillSep [ pattern', "=", expr ]
  where
    pattern' = pretty $ pvb_pat d
    expr     = pretty $ pvb_expr d

instance (Pretty Expression) => Pretty Value_binding where
  pretty = value_binding_PP
