{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Longident
  ( longidentPP,
  )
where

import Language.OCaml.Definitions.Parsing.Longident
  ( Longident (..),
  )
import Prettyprinter (Doc, Pretty (pretty), group)

longidentPP :: Longident -> Doc b
longidentPP = \case
  Lident s -> pretty s
  Ldot p s -> group $ pretty p <> "." <> pretty s
  Lapply _p _s -> error "TODO"

instance Pretty Longident where
  pretty = longidentPP
