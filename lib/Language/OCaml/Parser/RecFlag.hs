module Language.OCaml.Parser.RecFlag
  ( rec_flag_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens

rec_flag_P :: Parser ASTTypes.Rec_flag
rec_flag_P = choice
  [ rec_T *> return ASTTypes.Recursive
  , return ASTTypes.Nonrecursive
  ]
