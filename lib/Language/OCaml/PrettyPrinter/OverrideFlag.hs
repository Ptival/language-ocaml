{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.OverrideFlag
  ( overrideFlagPP,
  )
where

import Language.OCaml.Definitions.Parsing.ASTTypes
  ( OverrideFlag (..),
  )
import Prettyprinter (Doc, Pretty (pretty))

overrideFlagPP :: OverrideFlag -> Doc a
overrideFlagPP = \case
  Override -> "!"
  Fresh -> ""

instance Pretty OverrideFlag where
  pretty = overrideFlagPP
