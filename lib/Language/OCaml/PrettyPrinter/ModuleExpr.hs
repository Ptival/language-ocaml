{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ModuleExpr
  ( moduleExprPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( ModuleExpr (pmodDesc),
  )
import Language.OCaml.PrettyPrinter.ModuleExprDesc ()
import Prettyprinter (Doc, Pretty (pretty))

moduleExprPP :: ModuleExpr -> Doc a
moduleExprPP = pretty . pmodDesc

instance Pretty ModuleExpr where
  pretty = moduleExprPP
