{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.LabeledSimplePattern
  ( labeled_simple_pattern_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimplePattern

labeled_simple_pattern_P :: Parser (Arg_label, Maybe a, Pattern)
labeled_simple_pattern_P = choice
  [ -- TODO: lots of things
    do
      p <- simple_pattern_P
      return (Nolabel, Nothing, p)
  ]