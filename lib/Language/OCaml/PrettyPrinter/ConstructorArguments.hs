{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.ConstructorArguments
  ( constructorArgumentsPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.CoreType ()

constructorArgumentsPP :: ConstructorArguments -> Doc a
constructorArgumentsPP = \case
  PcstrTuple l -> case l of
    []  -> ""
    [x] -> fillSep [ "of", pretty x ]
    _   -> fillSep [ "of", encloseSep "" "" " * " (map pretty l) ]
  PcstrRecord _ -> error "TODO"

instance Pretty ConstructorArguments where
  pretty = constructorArgumentsPP
