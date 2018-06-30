{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.OverrideFlag
  ( overrideFlagPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

overrideFlagPP :: OverrideFlag -> Doc a
overrideFlagPP = \case
  Override -> "!"
  Fresh    -> ""

instance Pretty OverrideFlag where
  pretty = overrideFlagPP
