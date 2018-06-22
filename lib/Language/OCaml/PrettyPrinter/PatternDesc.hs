{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.PatternDesc
  ( pattern_desc_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleBinding ()
import Language.OCaml.PrettyPrinter.OpenDescription ()
import Language.OCaml.PrettyPrinter.TypeDeclaration ()

pattern_desc_PP :: Pretty Pattern => Pattern_desc -> Doc a
pattern_desc_PP = \case
  Ppat_any -> "_"
  Ppat_var v -> pretty v
  Ppat_alias _p _v -> error "TODO"
  Ppat_constant _c -> error "TODO"
  Ppat_constraint _ _ -> error "TODO"
  Ppat_tuple _l -> error "TODO"
  Ppat_construct i p -> case p of
    Nothing -> pretty i
    Just p' -> fillSep [ pretty i, pretty p' ]
  Ppat_or p1 p2 -> vcat [ pretty p1, fillSep [ pipe, pretty p2 ] ]
  Ppat_record _ _ -> error "TODO"
  Ppat_array _ -> error "TODO"

instance Pretty Pattern => Pretty Pattern_desc where
  pretty = pattern_desc_PP
