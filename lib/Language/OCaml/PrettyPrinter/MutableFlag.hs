{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.MutableFlag
  ( mutable_flag_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Language.OCaml.PrettyPrinter.OverrideFlag ()

mutable_flag_PP :: Mutable_flag -> Doc a
mutable_flag_PP = \case
  Immutable -> ""
  Mutable   -> fillCat [ "mutable", space ]

instance Pretty Mutable_flag where
  pretty = mutable_flag_PP
