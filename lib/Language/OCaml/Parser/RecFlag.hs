module Language.OCaml.Parser.RecFlag
  ( recFlagP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

recFlagP :: Parser ASTTypes.RecFlag
recFlagP = choice
  [ recT *> return ASTTypes.Recursive
  , return ASTTypes.NonRecursive
  ]
