{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.LabelDeclaration
  ( label_declaration_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType ()
import Language.OCaml.PrettyPrinter.MutableFlag ()

label_declaration_PP :: Label_declaration -> Doc a
label_declaration_PP d = fillCat [ name, space, colon, space, mutable, type' ]
  where
    name    = pretty $ pld_name d
    mutable = pretty $ pld_mutable d
    type'   = pretty $ pld_type d

instance Pretty Label_declaration where
  pretty = label_declaration_PP
