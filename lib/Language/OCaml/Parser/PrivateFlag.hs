module Language.OCaml.Parser.PrivateFlag
  ( privateFlagP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

privateFlagP :: Parser PrivateFlag
privateFlagP = choice
  [ privateT *> return Private
  , return Public
  ]
