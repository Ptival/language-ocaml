module Language.OCaml.Definitions.StdLib.Parsing
  ( rhsEndPos,
    rhsStart,
    rhsStartPos,
  )
where

import Language.OCaml.Definitions.StdLib.Lexing (Position (..))

rhsEndPos :: a -> Position
rhsEndPos _ =
  Position
    { posFName = "FIXME",
      posLNum = 0,
      posBOL = 0,
      posCNum = 0
    }

rhsStartPos :: a -> Position
rhsStartPos _ =
  Position
    { posFName = "FIXME",
      posLNum = 0,
      posBOL = 0,
      posCNum = 0
    }

rhsStart :: a -> Int
rhsStart _ = 0 -- FIXME
