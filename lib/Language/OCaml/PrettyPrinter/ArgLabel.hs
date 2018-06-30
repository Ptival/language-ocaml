{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ArgLabel
  ( argLabelPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.PrettyPrinter.Case ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

argLabelPP :: ArgLabel -> Doc a
argLabelPP = \case
  Nolabel -> ""
  _ -> error "TODO"

instance Pretty ArgLabel where
  pretty = argLabelPP
