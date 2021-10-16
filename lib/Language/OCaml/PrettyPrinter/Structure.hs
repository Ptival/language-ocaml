{-# LANGUAGE FlexibleContexts #-}

module Language.OCaml.PrettyPrinter.Structure
  ( structurePP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Payload,
    Structure,
  )
import Language.OCaml.PrettyPrinter.StructureItem ()
import Prettyprinter (Doc, Pretty (pretty), vcat)

structurePP :: Pretty Payload => Structure -> Doc a
structurePP = vcat . map pretty
