{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.CoreType
  ( core_type_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreTypeDesc ()

core_type_PP :: Core_type -> Doc a
core_type_PP = pretty . ptyp_desc

instance Pretty Core_type where
  pretty = core_type_PP
