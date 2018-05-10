{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.PatternCommaList
  ( pattern_comma_list_P
  ) where

import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators

pattern_comma_list_P :: Parser a -> Parser [a]
pattern_comma_list_P pattern_P =
  chainl1' pattern_P (comma_T *> return (flip (:))) (: [])
