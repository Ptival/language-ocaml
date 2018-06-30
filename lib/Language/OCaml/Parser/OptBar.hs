module Language.OCaml.Parser.OptBar
  ( optBarP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

optBarP :: Parser ()
optBarP = choice
  [ barT *> return ()
  , return ()
  ]
