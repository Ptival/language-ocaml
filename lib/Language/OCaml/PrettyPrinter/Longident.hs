{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Longident
  ( longidentPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree

longidentPP :: Longident -> Doc b
longidentPP = \case
  Lident s -> pretty s
  Ldot p s -> group $ pretty p <> "." <> pretty s
  Lapply _p _s -> error "TODO"

instance Pretty Longident where
  pretty = longidentPP
