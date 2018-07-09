{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Pat
  ( MkOpts(..)
  , attr
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

attr :: Pattern -> Attribute -> Pattern
attr d a = d { ppatAttributes = ppatAttributes d ++ [a] }

mk :: MkOpts -> PatternDesc -> Pattern
mk (MkOpts {..}) d = Pattern
  { ppatDesc       = d
  , ppatLoc        = loc
  , ppatAttributes = attrs
  }

data MkOpts = MkOpts
  { attrs :: [Attribute]
  , loc   :: Location
  }

instance Default MkOpts where
  def = MkOpts
    { attrs = []
    , loc   = defaultLoc
    }
