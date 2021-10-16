{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.RecFlag
  ( recFlagPP,
  )
where

import Language.OCaml.Definitions.Parsing.ASTTypes (RecFlag (..))
import Prettyprinter (Doc, Pretty (pretty))

recFlagPP :: RecFlag -> Doc a
recFlagPP = \case
  Recursive -> "rec "
  NonRecursive -> ""

instance Pretty RecFlag where
  pretty = recFlagPP
