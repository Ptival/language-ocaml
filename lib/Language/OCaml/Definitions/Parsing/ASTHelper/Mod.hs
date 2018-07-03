{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mod
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

mk :: MkOpts -> ModuleExprDesc -> ModuleExpr
mk (MkOpts {..}) d = ModuleExpr
  { pmodDesc       = d
  , pmodLoc        = loc
  , pmodAttributes = attrs
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
