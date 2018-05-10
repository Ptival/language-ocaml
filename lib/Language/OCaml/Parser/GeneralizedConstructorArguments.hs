module Language.OCaml.Parser.GeneralizedConstructorArguments
  ( generalized_constructor_arguments_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorArguments
import Language.OCaml.Parser.Tokens

generalized_constructor_arguments_P :: Parser (Constructor_arguments, Maybe a)
generalized_constructor_arguments_P = choice
  [ flip (,) Nothing <$> (of_T *> constructor_arguments_P)
    -- TODO: colon
  , return (Pcstr_tuple [], Nothing)
  ]
