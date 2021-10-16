{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.LabelDeclaration
  ( labelDeclarationPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( LabelDeclaration (pldMutable, pldName, pldType),
  )
import Language.OCaml.PrettyPrinter.CoreType ()
import Language.OCaml.PrettyPrinter.MutableFlag ()
import Prettyprinter (Doc, Pretty (pretty), colon, fillCat, space)

labelDeclarationPP :: LabelDeclaration -> Doc a
labelDeclarationPP d = fillCat [name, space, colon, space, mutable, type']
  where
    name = pretty $ pldName d
    mutable = pretty $ pldMutable d
    type' = pretty $ pldType d

instance Pretty LabelDeclaration where
  pretty = labelDeclarationPP
