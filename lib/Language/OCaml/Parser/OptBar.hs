module Language.OCaml.Parser.OptBar
  ( opt_bar_P
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

opt_bar_P :: Parser ()
opt_bar_P = choice
  [ bar_T *> return ()
  , return ()
  ]
