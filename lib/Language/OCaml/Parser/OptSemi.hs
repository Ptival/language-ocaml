module Language.OCaml.Parser.OptSemi
  ( optSemiP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

optSemiP :: Parser ()
optSemiP = choice
  [ semiT
  , return ()
  ]
