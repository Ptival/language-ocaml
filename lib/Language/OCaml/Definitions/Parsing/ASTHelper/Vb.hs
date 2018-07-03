{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Vb
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Location

mk :: MkOpts -> Pattern -> Expression -> ValueBinding
mk (MkOpts {..}) pat expr = ValueBinding
  { pvbPat        = pat
  , pvbExpr       = expr
  , pvbAttributes = addTextAttrs text . addDocsAttrs docs $ attrs
  , pvbLoc        = loc
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
