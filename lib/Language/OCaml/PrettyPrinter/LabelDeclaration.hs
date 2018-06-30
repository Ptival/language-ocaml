{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.LabelDeclaration
  ( labelDeclarationPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType ()
import Language.OCaml.PrettyPrinter.MutableFlag ()

labelDeclarationPP :: LabelDeclaration -> Doc a
labelDeclarationPP d = fillCat [ name, space, colon, space, mutable, type' ]
  where
    name    = pretty $ pldName d
    mutable = pretty $ pldMutable d
    type'   = pretty $ pldType d

instance Pretty LabelDeclaration where
  pretty = labelDeclarationPP
