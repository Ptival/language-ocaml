module Language.OCaml.Parser.NonrecFlag
  ( nonrec_flag_P
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

nonrec_flag_P :: Parser ASTTypes.Rec_flag
nonrec_flag_P = choice
  [ nonrec_T *> return ASTTypes.Nonrecursive
  , return ASTTypes.Recursive
  ]
