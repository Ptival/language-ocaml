{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.StructureItemDesc
  ( structure_item_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleBinding ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()
import Language.OCaml.PrettyPrinter.ValueBinding ()

structure_item_desc_PP :: Structure_item_desc -> Doc a
structure_item_desc_PP = \case
  Pstr_eval _e _a -> error "TODO"
  Pstr_value r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillSep [ "let", pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d ]) . pretty) t
  Pstr_type _r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillSep [ "type", pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d]) . pretty) t
  Pstr_module m -> pretty m
  Pstr_open o -> pretty o
  Pstr_attribute _a -> error "TODO"
  Pstr_extension _e _a -> error "TODO"

instance Pretty Structure_item_desc where
  pretty = structure_item_desc_PP