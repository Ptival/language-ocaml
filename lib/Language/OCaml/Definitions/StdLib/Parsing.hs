module Language.OCaml.Definitions.StdLib.Parsing
  ( rhs_start
  , rhs_start_pos
  ) where

import Language.OCaml.Definitions.StdLib.Lexing

rhs_start_pos :: a -> Position
rhs_start_pos _ = Position
  { pos_fname = "FIXME"
  , pos_lnum  = 0
  , pos_bol   = 0
  , pos_cnum  = 0
  }

rhs_start :: a -> Int
rhs_start _ = 0 -- FIXME
