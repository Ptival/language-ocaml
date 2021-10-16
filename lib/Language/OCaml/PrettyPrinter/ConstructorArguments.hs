{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ConstructorArguments
  ( constructorArgumentsPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( ConstructorArguments (..),
  )
import Language.OCaml.PrettyPrinter.CoreType ()
import Prettyprinter (Doc, Pretty (pretty), encloseSep, fillSep)

constructorArgumentsPP :: ConstructorArguments -> Doc a
constructorArgumentsPP = \case
  PcstrTuple l -> case l of
    [] -> ""
    [x] -> fillSep ["of", pretty x]
    _ -> fillSep ["of", encloseSep "" "" " * " (map pretty l)]
  PcstrRecord _ -> error "TODO"

instance Pretty ConstructorArguments where
  pretty = constructorArgumentsPP
