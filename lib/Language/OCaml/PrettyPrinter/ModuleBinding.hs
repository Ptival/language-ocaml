{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ModuleBinding
  ( moduleBindingPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ModuleExpr ()

moduleBindingPP :: ModuleBinding -> Doc a
moduleBindingPP d = fillSep [ "module", name, "=", expr ]
  where
    name = pretty $ pmbName d
    expr = pretty $ pmbExpr d

instance Pretty ModuleBinding where
  pretty = moduleBindingPP
