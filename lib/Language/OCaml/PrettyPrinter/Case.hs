{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.Case
  ( casePP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Pattern         ()

casePP :: (Pretty Expression) => Case -> Doc a
casePP d = fillCat [ pipe, space, lhs, guard, space, "->", space, rhs ]
  where
    lhs = pretty $ pcLHS d
    rhs = pretty $ pcRHS d
    guard = case pcGuard d of
      Nothing -> ""
      Just g  -> space <> fillSep [ "when", pretty g ]

instance Pretty Expression => Pretty Case where
  pretty = casePP
