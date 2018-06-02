{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ConstructorArguments
  ( constructor_arguments_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType ()

constructor_arguments_PP :: Constructor_arguments -> Doc a
constructor_arguments_PP = \case
  Pcstr_tuple l -> case l of
    []  -> ""
    [x] -> fillSep [ "of", pretty x ]
    _   -> fillSep [ "of", encloseSep "" "" " * " (map pretty l) ]

instance Pretty Constructor_arguments where
  pretty = constructor_arguments_PP
