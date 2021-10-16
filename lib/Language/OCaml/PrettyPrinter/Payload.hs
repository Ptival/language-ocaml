{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Payload
  ( payloadPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Payload (..),
    StructureItem,
  )
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()
import Language.OCaml.PrettyPrinter.Structure (structurePP)
import Prettyprinter (Doc, Pretty (pretty))

payloadPP ::
  --( Pretty SignatureItem
  ( Pretty StructureItem
  ) =>
  Payload ->
  Doc a
payloadPP = \case
  PStr s -> structurePP s
  PSig _s -> error "TODO" -- pretty s
  PTyp t -> pretty t
  PPat _p _e -> error "TODO"

instance--( Pretty SignatureItem

  ( Pretty StructureItem
  ) =>
  Pretty Payload
  where
  pretty = payloadPP
