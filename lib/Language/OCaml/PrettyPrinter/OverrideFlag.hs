{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.OverrideFlag
  ( override_flag_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

override_flag_PP :: Override_flag -> Doc a
override_flag_PP = \case
  Override -> "!"
  Fresh    -> ""

instance Pretty Override_flag where
  pretty = override_flag_PP
