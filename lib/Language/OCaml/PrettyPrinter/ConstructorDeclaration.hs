{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ConstructorDeclaration
  ( constructor_declaration_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ConstructorArguments ()

constructor_declaration_PP :: Constructor_declaration -> Doc a
constructor_declaration_PP d =
  fillSep [ pipe, name, args, res ]
  where
    name = pretty $ pcd_name d
    args = pretty $ pcd_args d
    res = case pcd_res d of
      Nothing -> ""
      Just _r -> "TODO: res Just"

instance Pretty Constructor_declaration where
  pretty = constructor_declaration_PP
