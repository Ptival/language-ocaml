{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.StructureItemDesc
  ( structureItemDescPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Payload,
    StructureItemDesc (..),
  )
import Language.OCaml.PrettyPrinter.Expression ()
import Language.OCaml.PrettyPrinter.ModuleBinding ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()
import Language.OCaml.PrettyPrinter.ValueBinding ()
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    fillCat,
    fillSep,
    vcat,
  )

structureItemDescPP ::
  (Pretty Payload) => StructureItemDesc -> Doc a
structureItemDescPP = \case
  PstrAttribute _a -> error "TODO"
  PstrClass _ -> error "TODO"
  PstrClassType _ -> error "TODO"
  PstrEval e _a -> pretty e
  PstrException _e -> error "TODO"
  PstrExtension _e _a -> error "TODO"
  PstrInclude _ -> error "TODO"
  PstrModType _ -> error "TODO"
  PstrModule m -> pretty m
  PstrOpen o -> pretty o
  PstrPrimitive _ -> error "TODO"
  PstrRecModule _ -> error "TODO"
  PstrType _r l -> case l of
    [] -> error "?"
    h : t ->
      vcat $
        fillSep ["type", pretty h] :
        map ((\d -> fillSep ["\nand", d]) . pretty) t
  PstrTypExt _ -> error "TODO"
  PstrValue r l -> case l of
    [] -> error "?"
    h : t ->
      vcat $
        []
          ++ [fillCat ["let ", pretty r, pretty h]]
          ++ map ((\d -> fillSep ["\nand", d]) . pretty) t

instance (Pretty Payload) => Pretty StructureItemDesc where
  pretty = structureItemDescPP
