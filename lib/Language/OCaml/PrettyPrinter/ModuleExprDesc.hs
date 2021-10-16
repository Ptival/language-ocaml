{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ModuleExprDesc
  ( moduleExprDescPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( ModuleExprDesc (..),
  )
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()
import Prettyprinter (Doc, Pretty (pretty))

moduleExprDescPP :: ModuleExprDesc -> Doc a
moduleExprDescPP = \case
  PmodIdent i -> pretty i
  PmodStructure _ -> error "TODO"
  PmodFunctor {} -> error "TODO"
  PmodApply _ _ -> error "TODO"
  PmodConstraint _ _ -> error "TODO"
  PmodUnpack _ -> error "TODO"
  PmodExtension _ -> error "TODO"

instance Pretty ModuleExprDesc where
  pretty = moduleExprDescPP
