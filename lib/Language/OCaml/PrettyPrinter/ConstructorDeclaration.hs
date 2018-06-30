{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ConstructorDeclaration
  ( constructorDeclarationPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorArguments ()

constructorDeclarationPP :: ConstructorDeclaration -> Doc a
constructorDeclarationPP d =
  fillSep [ pipe, name, args, res ]
  where
    name = pretty $ pcdName d
    args = pretty $ pcdArgs d
    res = case pcdRes d of
      Nothing -> ""
      Just _r -> "TODO: res Just"

instance Pretty ConstructorDeclaration where
  pretty = constructorDeclarationPP
