{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.Payload
  ( payload_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()

payload_PP ::
  --( Pretty Signature_item
  ( Pretty Structure_item
  ) => Payload -> Doc a
payload_PP = \case
  PStr s -> pretty s
  PSig s -> error "TODO" -- pretty s
  PTyp t -> pretty t
  PPat p e -> error "TODO"

instance --( Pretty Signature_item
         ( Pretty Structure_item
         ) => Pretty Payload where
  pretty = payload_PP
