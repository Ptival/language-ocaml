{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Location
  ( Location (..),
    none,
  )
where

import GHC.Generics (Generic)
import Language.OCaml.Definitions.StdLib.Lexing (Position (..))

data Location = Location
  { locStart :: Position,
    locEnd :: Position,
    locGhost :: Bool
  }
  deriving (Eq, Generic, Show)

inFile :: String -> Location
inFile name =
  Location
    { locStart = loc,
      locEnd = loc,
      locGhost = True
    }
  where
    loc =
      Position
        { posFName = name,
          posLNum = 1,
          posBOL = 0,
          posCNum = -1
        }

none :: Location
none = inFile "__none__"
