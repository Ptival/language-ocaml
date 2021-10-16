{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Expression
  ( expressionPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Expression (pexpDesc),
  )
import Language.OCaml.PrettyPrinter.ExpressionDesc ()
import Prettyprinter (Doc, Pretty (pretty))

expressionPP :: Expression -> Doc a
expressionPP = pretty . pexpDesc

instance Pretty Expression where
  pretty = expressionPP
