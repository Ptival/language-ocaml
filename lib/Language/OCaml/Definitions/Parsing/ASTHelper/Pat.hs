{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Pat
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

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
