{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.Variance
  ( variancePP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration       ()

variancePP :: Variance -> Doc a
variancePP = \case
  Contravariant -> "-"
  Covariant     -> "+"
  Invariant     -> ""

instance Pretty Variance where
  pretty = variancePP
