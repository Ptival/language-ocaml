module Language.OCaml.Parser.OverrideFlag
  ( overrideFlagP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

overrideFlagP :: Parser ASTTypes.OverrideFlag
overrideFlagP = choice
  [ bangT *> return ASTTypes.Override
  , return ASTTypes.Fresh
  ]
