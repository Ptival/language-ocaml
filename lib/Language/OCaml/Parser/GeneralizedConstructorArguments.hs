module Language.OCaml.Parser.GeneralizedConstructorArguments
  ( generalized_constructor_arguments_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorArguments
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

generalized_constructor_arguments_P :: Parser Core_type -> Parser (Constructor_arguments, Maybe a)
generalized_constructor_arguments_P core_type_P = choice
  [ flip (,) Nothing <$> (of_T *> constructor_arguments_P core_type_P)
    -- TODO: colon
  , return (Pcstr_tuple [], Nothing)
  ]
