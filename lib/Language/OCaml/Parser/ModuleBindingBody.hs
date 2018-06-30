module Language.OCaml.Parser.ModuleBindingBody
  ( moduleBindingBodyP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModuleExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

moduleBindingBodyP :: Parser ModuleExpr
moduleBindingBodyP = choice
  [ do
    try $ equalT
    moduleExprP
    -- TODO: rest
  ]
