{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Sig
  ( AttributeOpts (..),
    MkOpts (..),
    attribute,
    mk,
    text,
  )
where

import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docstring,
    docstringBody,
    docstringLoc,
    textAttr,
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    SignatureItem (..),
    SignatureItemDesc (PsigAttribute),
  )

attribute :: AttributeOpts -> Attribute -> SignatureItem
attribute (AttributeOpts {..}) a = mk (def {loc}) (PsigAttribute a)

newtype AttributeOpts = AttributeOpts
  { loc :: Location
  }

instance Default AttributeOpts where
  def =
    AttributeOpts
      { loc = defaultLoc
      }

mk :: MkOpts -> SignatureItemDesc -> SignatureItem
mk (MkOpts {..}) d =
  SignatureItem
    { psigDesc = d,
      psigLoc = loc
    }

newtype MkOpts = MkOpts
  { loc :: Location
  }

instance Default MkOpts where
  def =
    MkOpts
      { loc = defaultLoc
      }

text :: [Docstring] -> [SignatureItem]
text txt =
  let fTxt = filter (\ds -> docstringBody ds /= "") txt
   in map (\ds -> attribute (def {loc = docstringLoc ds}) (textAttr ds)) fTxt
