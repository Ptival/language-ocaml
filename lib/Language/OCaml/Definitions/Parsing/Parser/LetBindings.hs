{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBindings
  ( Let_bindings(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.Parser.LetBinding

data Let_bindings = Let_bindings
  { lbs_bindings  :: [Let_binding]
  , lbs_rec       :: Rec_flag
  , lbs_extension :: Maybe (Loc String)
  , lbs_loc       :: Location
  }
  deriving (Eq, Generic, Show)
