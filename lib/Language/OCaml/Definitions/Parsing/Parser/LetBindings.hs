{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBindings
  ( LetBindings (..),
  )
where

import GHC.Generics (Generic)
import Language.OCaml.Definitions.Parsing.ASTTypes (Loc, RecFlag)
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.Parser.LetBinding
  ( LetBinding,
  )

data LetBindings = LetBindings
  { lbsBindings :: [LetBinding],
    lbsRec :: RecFlag,
    lbsExtension :: Maybe (Loc String),
    lbsLoc :: Location
  }
  deriving (Eq, Generic, Show)
