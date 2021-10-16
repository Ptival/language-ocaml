{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.OCaml.PrettyPrinter.Pattern
  ( patternPP,
  )
where

import Language.OCaml.Definitions.Parsing.ParseTree
  ( Pattern (ppatDesc),
  )
import Language.OCaml.PrettyPrinter.PatternDesc ()
import Prettyprinter (Doc, Pretty (pretty))

patternPP :: Pattern -> Doc a
patternPP = pretty . ppatDesc

instance Pretty Pattern where
  pretty = patternPP
