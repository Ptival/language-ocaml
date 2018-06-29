module Language.OCaml.Parser.RecFlag
  ( rec_flag_P
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

rec_flag_P :: Parser ASTTypes.Rec_flag
rec_flag_P = choice
  [ rec_T *> return ASTTypes.Recursive
  , return ASTTypes.NonRecursive
  ]
