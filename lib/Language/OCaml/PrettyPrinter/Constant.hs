{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Constant
  ( constant_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree

constant_PP :: Constant -> Doc a
constant_PP = \case
  Pconst_integer s Nothing  -> pretty s
  Pconst_integer s (Just c) -> "0" <> pretty c <> pretty s
  Pconst_char c -> pretty c
  Pconst_string s Nothing  -> dquote <> pretty s <> dquote
  Pconst_string s (Just q) -> error "TODO"
  Pconst_float s Nothing  -> pretty s
  Pconst_float s (Just c) -> error "TODO"

instance Pretty Constant where
  pretty = constant_PP
