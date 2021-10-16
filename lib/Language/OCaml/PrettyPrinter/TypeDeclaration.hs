{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.TypeDeclaration
  ( typeDeclarationPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorDeclaration ()
import Language.OCaml.PrettyPrinter.LabelDeclaration ()
import Language.OCaml.PrettyPrinter.Variance ()
import Prettyprinter

typeDeclarationPP :: (Pretty Payload) => TypeDeclaration -> Doc a
typeDeclarationPP d =
  fillCat [params, name, space, "=", body, attrs]
  where
    params = hcat $ map prettyParam $ ptypeParams d
    prettyParam (t, v) = fillCat [pretty v, pretty t, space]
    name = pretty $ ptypeName d
    manifest = maybe "FIXME manifest" pretty (ptypeManifest d)
    body = case ptypeKind d of
      PtypeAbstract -> fillCat [space, manifest]
      PtypeVariant l -> case l of
        [] -> fillCat [space, pipe]
        _ -> nest 2 $ fillCat [line, vcat $ map pretty l]
      PtypeRecord l ->
        space
          <> nest
            2
            ( line
                <> encloseSep
                  (lbrace <> space)
                  (line <> rbrace)
                  (semi <> space)
                  (map pretty l)
            )
      PtypeOpen -> "TODO: Ptype_open"
    attrs = case ptypeAttributes d of
      [] -> ""
      as -> line <> (vcat . map displayAttr $ as)
        where
          displayAttr (s, p) = fillSep ["[@@", pretty s, pretty p, "]"]

instance (Pretty Payload) => Pretty TypeDeclaration where
  pretty = typeDeclarationPP
