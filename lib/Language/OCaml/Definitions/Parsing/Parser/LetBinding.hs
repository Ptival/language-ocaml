{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBinding
  ( Let_binding(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree

data Let_binding = Let_binding
  { lb_pattern    :: Pattern
  , lb_expression :: Expression
  , lb_attributes :: Attributes
  , lb_docs       :: Docs
  , lb_text       :: Text
  , lb_loc        :: Location
  }
  deriving (Eq, Generic, Show)
