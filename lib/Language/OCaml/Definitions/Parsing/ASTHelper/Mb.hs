{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mb
  ( MkOpts (..),
    mk,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes (Loc)
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
    ModuleBinding (..),
    ModuleExpr,
  )

mk :: MkOpts -> Loc String -> ModuleExpr -> ModuleBinding
mk (MkOpts {..}) name expr =
  ModuleBinding
    { pmbName = name,
      pmbExpr = expr,
      pmbAttributes = addTextAttrs text . addDocsAttrs docs $ attrs,
      pmbLoc = loc
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
