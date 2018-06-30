{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Expression
  ( expressionPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.ExpressionDesc ()

expressionPP :: Expression -> Doc a
expressionPP = pretty . pexpDesc

instance Pretty Expression where
  pretty = expressionPP
