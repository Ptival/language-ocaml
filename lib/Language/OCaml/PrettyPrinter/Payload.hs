{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.Payload
  ( payloadPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()
import Language.OCaml.PrettyPrinter.Structure

payloadPP ::
  --( Pretty SignatureItem
  ( Pretty StructureItem
  ) => Payload -> Doc a
payloadPP = \case
  PStr s -> structurePP s
  PSig _s -> error "TODO" -- pretty s
  PTyp t -> pretty t
  PPat _p _e -> error "TODO"

instance --( Pretty SignatureItem
         ( Pretty StructureItem
         ) => Pretty Payload where
  pretty = payloadPP
