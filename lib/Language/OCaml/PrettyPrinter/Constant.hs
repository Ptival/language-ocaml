{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Constant
  ( constantPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Constant (..),
  )
import Prettyprinter (Doc, Pretty (pretty), dquote)

constantPP :: Constant -> Doc a
constantPP = \case
  PconstInteger s Nothing -> pretty s
  PconstInteger s (Just c) -> "0" <> pretty c <> pretty s
  PconstChar c -> pretty c
  PconstString s Nothing -> dquote <> pretty s <> dquote
  PconstString _s (Just _q) -> error "TODO"
  PconstFloat s Nothing -> pretty s
  PconstFloat _s (Just _c) -> error "TODO"

instance Pretty Constant where
  pretty = constantPP
