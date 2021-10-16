{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Variance
  ( variancePP,
  )
where

import Language.OCaml.Definitions.Parsing.ASTTypes (Variance (..))
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()
import Prettyprinter (Doc, Pretty (pretty))

variancePP :: Variance -> Doc a
variancePP = \case
  Contravariant -> "-"
  Covariant -> "+"
  Invariant -> ""

instance Pretty Variance where
  pretty = variancePP
