{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBindings
  ( LetBindings(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.Parser.LetBinding

data LetBindings = LetBindings
  { lbsBindings  :: [LetBinding]
  , lbsRec       :: RecFlag
  , lbsExtension :: Maybe (Loc String)
  , lbsLoc       :: Location
  }
  deriving (Eq, Generic, Show)
