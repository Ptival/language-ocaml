{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.TypeDeclaration
  ( type_declaration_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()

type_declaration_PP :: Type_declaration -> Doc a
type_declaration_PP d =
  fillCat [ params, name, space, "=", rest ]
  where
    params = ""
    name = pretty $ ptype_name d
    manifest = case ptype_manifest d of
      Nothing -> error "TODO"
      Just t -> pretty t
    rest = case ptype_kind d of
      Ptype_abstract -> fillCat [ space, manifest ]
      Ptype_variant l -> case l of
        [] -> fillCat [ space, pipe ]
        _  -> nest 2 $ fillCat [ line, vcat $ map pretty l ]
      Ptype_record l ->
        space <> (nest 2 $ encloseSep lbrace rbrace (line' <> semi <> space) (map pretty l))
      Ptype_open -> "TODO: Ptype_open"

instance Pretty Type_declaration where
  pretty = type_declaration_PP
