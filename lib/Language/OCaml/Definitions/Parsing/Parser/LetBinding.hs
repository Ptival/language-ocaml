{-# LANGUAGE DeriveGeneric #-}

module Language.OCaml.Definitions.Parsing.Parser.LetBinding
  ( LetBinding (..),
  )
where

import GHC.Generics (Generic)
import Language.OCaml.Definitions.Parsing.Docstrings (Docs, Text)
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attributes,
    Expression,
    Pattern,
  )

data LetBinding = LetBinding
  { lbPattern :: Pattern,
    lbExpression :: Expression,
    lbAttributes :: Attributes,
    lbDocs :: Docs,
    lbText :: Text,
    lbLoc :: Location
  }
  deriving (Eq, Generic, Show)
