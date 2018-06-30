module Language.OCaml.Parser.NonRecFlag
  ( nonrecFlagP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

nonrecFlagP :: Parser ASTTypes.RecFlag
nonrecFlagP = choice
  [ nonrecT *> return ASTTypes.NonRecursive
  , return ASTTypes.Recursive
  ]
