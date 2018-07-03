{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mb
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes hiding (loc)
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

mk :: MkOpts -> Loc String -> ModuleExpr -> ModuleBinding
mk (MkOpts {..}) name expr = ModuleBinding
  { pmbName       = name
  , pmbExpr       = expr
  , pmbAttributes = addTextAttrs text . addDocsAttrs docs $ attrs
  , pmbLoc        = loc
  }

data MkOpts = MkOpts
  { attrs :: [Attribute]
  , docs  :: Docs
  , loc   :: Location
  , text  :: [Docstring]
  }

instance Default MkOpts where
  def = MkOpts
    { attrs = []
    , docs  = emptyDocs
    , loc   = defaultLoc
    , text  = []
    }
