{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.OCaml.PrettyPrinter.ValueBinding
  ( valueBindingPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.Pattern ()

valueBindingPP :: (Pretty Expression) => ValueBinding -> Doc a
valueBindingPP d = fillSep [ pattern', "=", expr ]
  where
    pattern' = pretty $ pvbPat d
    expr     = pretty $ pvbExpr d

instance (Pretty Expression) => Pretty ValueBinding where
  pretty = valueBindingPP
