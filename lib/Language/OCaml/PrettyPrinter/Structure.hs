{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.OCaml.PrettyPrinter.Structure
  ( structure_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.StructureItem ()

structure_PP :: Structure -> Doc a
structure_PP = vcat . map pretty
