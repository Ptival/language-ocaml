{-# LANGUAGE FlexibleContexts #-}

module Language.OCaml.PrettyPrinter.Structure
  ( structure_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.StructureItem ()

structure_PP :: (Pretty Payload) => Structure -> Doc a
structure_PP = vcat . map pretty
