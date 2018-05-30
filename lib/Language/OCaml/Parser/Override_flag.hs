module Language.OCaml.Parser.Override_flag
  ( override_flag_P
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

override_flag_P :: Parser ASTTypes.Override_flag
override_flag_P = choice
  [ bang_T *> return ASTTypes.Override
  , return ASTTypes.Fresh
  ]
