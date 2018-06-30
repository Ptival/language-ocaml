{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleExprDesc
  ( moduleExprDescPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

moduleExprDescPP :: ModuleExprDesc -> Doc a
moduleExprDescPP = \case
  PmodIdent i -> pretty i

instance Pretty ModuleExprDesc where
  pretty = moduleExprDescPP
