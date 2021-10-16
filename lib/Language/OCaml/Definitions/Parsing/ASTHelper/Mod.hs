{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mod
  ( MkOpts (..),
    attr,
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    ModuleExpr (..),
    ModuleExprDesc,
  )

attr :: ModuleExpr -> Attribute -> ModuleExpr
attr d a = d {pmodAttributes = pmodAttributes d ++ [a]}

mk :: MkOpts -> ModuleExprDesc -> ModuleExpr
mk (MkOpts {..}) d =
  ModuleExpr
    { pmodDesc = d,
      pmodLoc = loc,
      pmodAttributes = attrs
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    loc :: Location
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        loc = defaultLoc
      }
