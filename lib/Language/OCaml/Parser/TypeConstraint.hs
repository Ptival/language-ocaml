module Language.OCaml.Parser.TypeConstraint
  ( typeConstraintP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

typeConstraintP :: Parser (Maybe CoreType, Maybe CoreType)
typeConstraintP = choice
  [ do
    t1 <- try $ do
      colonT
      t1 <- coreTypeP
      colonGreaterT
      return t1
    t2 <- coreTypeP
    return (Just t1, Just t2)
  , try $ do
    colonT
    t <- coreTypeP
    return (Just t, Nothing)
  , do
    try $ colonGreaterT
    t <- coreTypeP
    return (Nothing, Just t)
  ]
