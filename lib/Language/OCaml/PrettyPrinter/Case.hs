{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Case
  ( casePP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Case (pcGuard, pcLHS, pcRHS),
    Expression,
  )
import Language.OCaml.PrettyPrinter.Pattern ()
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    fillCat,
    fillSep,
    pipe,
    space,
  )

casePP :: (Pretty Expression) => Case -> Doc a
casePP d = fillCat [pipe, space, lhs, guard, space, "->", space, rhs]
  where
    lhs = pretty $ pcLHS d
    rhs = pretty $ pcRHS d
    guard = case pcGuard d of
      Nothing -> ""
      Just g -> space <> fillSep ["when", pretty g]

instance Pretty Expression => Pretty Case where
  pretty = casePP
