{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.StructureItemDesc
  ( structureItemDescPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Expression ()
import Language.OCaml.PrettyPrinter.ModuleBinding ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()
import Language.OCaml.PrettyPrinter.ValueBinding ()

structureItemDescPP ::
  (Pretty Payload) => StructureItemDesc -> Doc a
structureItemDescPP = \case
  PstrEval e _a -> pretty e
  PstrValue r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillCat [ "let ", pretty r, pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d ]) . pretty) t
  PstrType _r l -> case l of
    []  -> error "?"
    h:t -> vcat $ fillSep [ "type", pretty h ]
                    : map ((\ d -> fillSep [ "\nand", d]) . pretty) t
  PstrModule m -> pretty m
  PstrOpen o -> pretty o
  PstrAttribute _a -> error "TODO"
  PstrExtension _e _a -> error "TODO"
  PstrException _e -> error "TODO"

instance (Pretty Payload) => Pretty StructureItemDesc where
  pretty = structureItemDescPP
