{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Opn
  ( MkOpts(..)
  , mk
  ) where

import Data.Default

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes         hiding (loc)
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.ParseTree

mk :: MkOpts -> Loc Longident -> OpenDescription
mk (MkOpts {..}) lid =
  OpenDescription
  { popenLid        = lid
  , popenOverride   = override
  , popenLoc        = loc
  , popenAttributes = addDocsAttrs docs attrs
  }

data MkOpts = MkOpts
  { attrs    :: [Attribute]
  , docs     :: Docs
  , loc      :: Location
  , override :: OverrideFlag
  }

instance Default MkOpts where
  def = MkOpts
    { attrs    = []
    , docs     = emptyDocs
    , loc      = defaultLoc
    , override = Fresh
    }
