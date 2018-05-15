{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ExprCommaList
  ( expr_comma_list_P
  ) where

import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators

expr_comma_list_P :: Parser Expression -> Parser [Expression]
expr_comma_list_P expr_P = leftRecursive
  [ do
    e1 <- expr_P
    comma_T
    e2 <- expr_P
    return [e2, e1]
  ]
  [ do
    comma_T
    e <- expr_P
    return $ (:) e
  ]
