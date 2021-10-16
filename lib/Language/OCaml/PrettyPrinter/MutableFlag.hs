{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.MutableFlag
  ( mutableFlagPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( MutableFlag (..),
  )
import Language.OCaml.PrettyPrinter.CoreType ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.OverrideFlag ()
import Prettyprinter (Doc, Pretty (pretty), fillCat, space)

mutableFlagPP :: MutableFlag -> Doc a
mutableFlagPP = \case
  Immutable -> ""
  Mutable -> fillCat ["mutable", space]

instance Pretty MutableFlag where
  pretty = mutableFlagPP
