{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.PrettyPrinter.Pattern
  ( patternPP
  ) where

import Data.Text.Prettyprint.Doc

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter.PatternDesc ()

patternPP :: Pattern -> Doc a
patternPP = pretty . ppatDesc

instance Pretty Pattern where
  pretty = patternPP
