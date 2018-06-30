{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBinding
  ( LetBinding(..)
  ) where

import GHC.Generics

import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree

data LetBinding = LetBinding
  { lbPattern    :: Pattern
  , lbExpression :: Expression
  , lbAttributes :: Attributes
  , lbDocs       :: Docs
  , lbText       :: Text
  , lbLoc        :: Location
  }
  deriving (Eq, Generic, Show)
