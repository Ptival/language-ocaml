{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Mtd
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
    Text,
    addDocsAttrs,
    addTextAttrs,
    emptyDocs,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree as ParseTree
  ( Attribute,
    ModuleType,
    ModuleTypeDeclaration (..),
  )
import Prelude hiding (sequence)

mk :: MkOpts -> Maybe ModuleType -> Loc String -> ModuleTypeDeclaration
mk (MkOpts {..}) typ name =
  ModuleTypeDeclaration
    { pmtdName = name,
      pmtdType = typ,
      pmtdAttributes = addTextAttrs text $ addDocsAttrs docs $ attrs,
      pmtdLoc = loc
    }

data MkOpts = MkOpts
  { attrs :: [Attribute],
    docs :: Docs,
    loc :: Location,
    text :: Text
  }

instance Default MkOpts where
  def =
    MkOpts
      { attrs = [],
        docs = emptyDocs,
        loc = defaultLoc,
        text = []
      }
