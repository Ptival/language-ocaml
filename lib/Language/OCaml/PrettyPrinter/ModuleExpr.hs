{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleExpr
  ( moduleExprPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleExprDesc ()

moduleExprPP :: ModuleExpr -> Doc a
moduleExprPP = pretty . pmodDesc

instance Pretty ModuleExpr where
  pretty = moduleExprPP
