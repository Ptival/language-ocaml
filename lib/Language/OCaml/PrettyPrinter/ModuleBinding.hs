{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ModuleBinding
  ( moduleBindingPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( ModuleBinding (pmbExpr, pmbName),
  )
import Language.OCaml.PrettyPrinter.ModuleExpr ()
import Prettyprinter (Doc, Pretty (pretty), fillSep)

moduleBindingPP :: ModuleBinding -> Doc a
moduleBindingPP d = fillSep ["module", name, "=", expr]
  where
    name = pretty $ pmbName d
    expr = pretty $ pmbExpr d

instance Pretty ModuleBinding where
  pretty = moduleBindingPP
