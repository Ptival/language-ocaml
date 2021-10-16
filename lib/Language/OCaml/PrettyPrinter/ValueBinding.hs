{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.ValueBinding
  ( valueBindingPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Expression,
    ValueBinding (pvbExpr, pvbPat),
  )
import Language.OCaml.PrettyPrinter.Pattern ()
import Prettyprinter (Doc, Pretty (pretty), fillSep)

valueBindingPP :: (Pretty Expression) => ValueBinding -> Doc a
valueBindingPP d = fillSep [pattern', "=", expr]
  where
    pattern' = pretty $ pvbPat d
    expr = pretty $ pvbExpr d

instance (Pretty Expression) => Pretty ValueBinding where
  pretty = valueBindingPP
