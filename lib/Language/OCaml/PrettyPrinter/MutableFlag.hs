{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.MutableFlag
  ( mutableFlagPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType        ()
import Language.OCaml.PrettyPrinter.Loc             ()
import Language.OCaml.PrettyPrinter.Longident       ()
import Language.OCaml.PrettyPrinter.OverrideFlag    ()

mutableFlagPP :: MutableFlag -> Doc a
mutableFlagPP = \case
  Immutable -> ""
  Mutable   -> fillCat [ "mutable", space ]

instance Pretty MutableFlag where
  pretty = mutableFlagPP
