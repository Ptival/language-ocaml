module Language.OCaml.Parser.OptSemi
  ( opt_semi_P
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

opt_semi_P :: Parser ()
opt_semi_P = choice
  [ semi_T
  , return ()
  ]
