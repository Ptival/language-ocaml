{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.StructureItem
  ( structureItemPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.StructureItemDesc ()

structureItemPP :: (Pretty Payload) => StructureItem -> Doc a
structureItemPP = pretty . pstrDesc

instance (Pretty Payload) => Pretty StructureItem where
  pretty = structureItemPP
