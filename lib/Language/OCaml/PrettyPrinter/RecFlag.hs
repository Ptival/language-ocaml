{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.RecFlag
  ( recFlagPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes

recFlagPP :: RecFlag -> Doc a
recFlagPP = \case
  Recursive    -> "rec "
  NonRecursive -> ""

instance Pretty RecFlag where
  pretty = recFlagPP
