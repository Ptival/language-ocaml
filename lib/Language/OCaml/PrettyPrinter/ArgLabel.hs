{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ArgLabel
  ( arg_label_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.PrettyPrinter.Case ()
import Language.OCaml.PrettyPrinter.Loc ()
import Language.OCaml.PrettyPrinter.Longident ()

arg_label_PP :: Arg_label -> Doc a
arg_label_PP = \case
  Nolabel -> ""
  _ -> error "TODO"

instance Pretty Arg_label where
  pretty = arg_label_PP
