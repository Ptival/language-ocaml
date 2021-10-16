{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Vb
  ( MkOpts (..),
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs,
    Docstring,
    addDocsAttrs,
    addTextAttrs,
    emptyDocs,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    Expression,
    Pattern,
    ValueBinding (..),
  )

mk :: MkOpts -> Pattern -> Expression -> ValueBinding
mk (MkOpts {..}) pat expr =
  ValueBinding
    { pvbPat = pat,
      pvbExpr = expr,
      pvbAttributes = addTextAttrs text . addDocsAttrs docs $ attrs,
      pvbLoc = loc
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    text :: [Docstring]
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        text = []
      }
