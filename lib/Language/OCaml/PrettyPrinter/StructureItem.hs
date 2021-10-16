{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.StructureItem
  ( structureItemPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Payload,
    StructureItem (pstrDesc),
  )
import Language.OCaml.PrettyPrinter.StructureItemDesc ()
import Prettyprinter (Doc, Pretty (pretty))

structureItemPP :: (Pretty Payload) => StructureItem -> Doc a
structureItemPP = pretty . pstrDesc

instance (Pretty Payload) => Pretty StructureItem where
  pretty = structureItemPP
