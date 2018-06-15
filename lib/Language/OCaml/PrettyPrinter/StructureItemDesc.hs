{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.StructureItemDesc
  ( structure_item_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Expression ()
import Language.OCaml.PrettyPrinter.ModuleBinding ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()
import Language.OCaml.PrettyPrinter.ValueBinding ()

structure_item_desc_PP ::
  (Pretty Payload) => Structure_item_desc -> Doc a
structure_item_desc_PP = \case
  Pstr_eval e _a -> pretty e
  Pstr_value r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillCat [ "let ", pretty r, pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d ]) . pretty) t
  Pstr_type _r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillSep [ "type", pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d]) . pretty) t
  Pstr_module m -> pretty m
  Pstr_open o -> pretty o
  Pstr_attribute _a -> error "TODO"
  Pstr_extension _e _a -> error "TODO"
  Pstr_exception _e -> error "TODO"

instance (Pretty Payload) => Pretty Structure_item_desc where
  pretty = structure_item_desc_PP
