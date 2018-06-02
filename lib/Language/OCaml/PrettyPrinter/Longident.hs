{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  Ldot p s -> group $ pretty p <> "." <> pretty s
  Lapply _p _s -> error "TODO"

instance Pretty Longident where
  pretty = longident_PP
