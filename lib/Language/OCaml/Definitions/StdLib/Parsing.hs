module Language.OCaml.Definitions.StdLib.Parsing
  ( rhsStart
  , rhsStartPos
  ) where

import Language.OCaml.Definitions.StdLib.Lexing

rhsStartPos :: a -> Position
rhsStartPos _ = Position
  { posFName = "FIXME"
  , posLNum  = 0
  , posBOL   = 0
  , posCNum  = 0
  }

rhsStart :: a -> Int
rhsStart _ = 0 -- FIXME
