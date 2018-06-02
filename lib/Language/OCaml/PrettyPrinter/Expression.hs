{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Expression
  ( expression_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ExpressionDesc ()

expression_PP :: Expression -> Doc a
expression_PP = pretty . pexp_desc

instance Pretty Expression where
  pretty = expression_PP
