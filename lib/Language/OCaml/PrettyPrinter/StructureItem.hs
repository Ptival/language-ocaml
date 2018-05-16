{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.StructureItem
  ( structure_item_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.StructureItemDesc ()

structure_item_PP :: (Pretty Payload) => Structure_item -> Doc a
structure_item_PP = pretty . pstr_desc

instance (Pretty Payload) => Pretty Structure_item where
  pretty = structure_item_PP
