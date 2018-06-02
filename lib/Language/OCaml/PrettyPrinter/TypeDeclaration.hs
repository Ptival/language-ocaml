{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.TypeDeclaration
  ( type_declaration_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()
import Language.OCaml.PrettyPrinter.Variance ()

type_declaration_PP :: (Pretty Payload) => Type_declaration -> Doc a
type_declaration_PP d =
  fillCat [ params, name, space, "=", body, attrs ]
  where
    params = hcat $ map prettyParam $ ptype_params d
    prettyParam (t, v) = fillCat [ pretty v, pretty t, space ]
    name = pretty $ ptype_name d
    manifest = case ptype_manifest d of
      Nothing -> error "TODO"
      Just t -> pretty t
    body = case ptype_kind d of
      Ptype_abstract -> fillCat [ space, manifest ]
      Ptype_variant l -> case l of
        [] -> fillCat [ space, pipe ]
        _  -> nest 2 $ fillCat [ line, vcat $ map pretty l ]
      Ptype_record l ->
        space
        <> (nest 2
            $ line
            <> encloseSep
            (lbrace <> space)
            (line <> rbrace)
            (semi <> space)
            (map pretty l)
           )
      Ptype_open -> "TODO: Ptype_open"
    attrs = case ptype_attributes d of
      [] -> ""
      as -> line <> (vcat . map displayAttr $ as)
        where
          displayAttr (s, p) = fillSep [ "[@@", pretty s, pretty p, "]" ]

instance (Pretty Payload) => Pretty Type_declaration where
  pretty = type_declaration_PP
