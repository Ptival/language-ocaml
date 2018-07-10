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
  PmodStructure _ -> error "TODO"
  PmodFunctor _ _ _ -> error "TODO"
  PmodApply _ _ -> error "TODO"
  PmodConstraint _ _ -> error "TODO"
  PmodUnpack _ -> error "TODO"
  PmodExtension _ -> error "TODO"

instance Pretty ModuleExprDesc where
  pretty = moduleExprDescPP
