{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ConstructorDeclaration
  ( constructorDeclarationPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( ConstructorDeclaration (pcdArgs, pcdName, pcdRes),
  )
import Language.OCaml.PrettyPrinter.ConstructorArguments ()
import Prettyprinter (Doc, Pretty (pretty), fillSep, pipe)

constructorDeclarationPP :: ConstructorDeclaration -> Doc a
constructorDeclarationPP d =
  fillSep [pipe, name, args, res]
  where
    name = pretty $ pcdName d
    args = pretty $ pcdArgs d
    res = case pcdRes d of
      Nothing -> ""
      Just _r -> "TODO: res Just"

instance Pretty ConstructorDeclaration where
  pretty = constructorDeclarationPP
