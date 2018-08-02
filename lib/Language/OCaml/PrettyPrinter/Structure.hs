{-# LANGUAGE FlexibleContexts #-}

module Language.OCaml.PrettyPrinter.Structure
  ( structurePP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.StructureItem   ()

structurePP :: (Pretty Payload) => Structure -> Doc a
structurePP = vcat . map pretty
