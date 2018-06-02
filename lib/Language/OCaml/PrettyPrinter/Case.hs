{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.Case
  ( case_PP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Pattern ()

case_PP :: (Pretty Expression) => Case -> Doc a
case_PP d = fillCat [ pipe, space, lhs, guard, space, "->", space, rhs ]
  where
    lhs = pretty $ pc_lhs d
    rhs = pretty $ pc_rhs d
    guard = case pc_guard d of
      Nothing -> ""
      Just g  -> space <> fillSep [ "when", pretty g ]

instance Pretty Expression => Pretty Case where
  pretty = case_PP
