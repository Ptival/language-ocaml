{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Longident
  ( longident_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree

longident_PP :: Longident -> Doc b
longident_PP = \case
  Lident s -> pretty s
  Ldot p s -> fillCat [pretty p, ".", pretty s]
  Lapply p s -> fillCat [pretty p, " ", pretty s]

instance Pretty Longident where
  pretty = longident_PP
