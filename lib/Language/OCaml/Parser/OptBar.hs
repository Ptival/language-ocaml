{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.OptBar
  ( opt_bar_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens

opt_bar_P :: Parser ()
opt_bar_P = choice
  [ bar_T *> return ()
  , return ()
  ]
