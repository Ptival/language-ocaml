{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Val
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

mk :: MkOpts -> Loc String -> CoreType -> ValueDescription
mk (MkOpts {..}) name typ = ValueDescription
  { pvalName       = name
  , pvalType       = typ
  , pvalAttributes = addDocsAttrs docs attrs
  , pvalLoc        = loc
  , pvalPrim       = prim
  }

data MkOpts = MkOpts
  { attrs :: [Attribute]
  , docs  :: Docs
  , loc   :: Location
  , prim  :: [String]
  }

instance Default MkOpts where
  def = MkOpts
    { attrs = []
    , docs  = emptyDocs
    , loc   = defaultLoc
    , prim  = []
    }
