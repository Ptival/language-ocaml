{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Pattern
  ( pattern_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.PatternDesc ()

pattern_PP :: Pattern -> Doc a
pattern_PP = pretty . ppat_desc

instance Pretty Pattern where
  pretty = pattern_PP
