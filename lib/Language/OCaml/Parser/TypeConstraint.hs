module Language.OCaml.Parser.TypeConstraint
  ( type_constraint_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.Tokens

type_constraint_P :: Parser (Maybe Core_type, Maybe Core_type)
type_constraint_P = choice
  [ do
    t1 <- try $ do
      colon_T
      t1 <- core_type_P
      colon_greater_T
      return t1
    t2 <- core_type_P
    return (Just t1, Just t2)
  , try $ do
    colon_T
    t <- core_type_P
    return (Just t, Nothing)
  , do
    try $ colon_greater_T
    t <- core_type_P
    return (Nothing, Just t)
  ]
