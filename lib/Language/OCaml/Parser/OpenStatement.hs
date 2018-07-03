{-# LANGUAGE NamedFieldPuns #-}

module Language.OCaml.Parser.OpenStatement
  ( openStatementP
  ) where

import Data.Default
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTHelper.Opn as Opn
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common (mkRHS, symbolRLoc, symbolDocs)
-- import Language.OCaml.Parser.ExtAttributes
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.OverrideFlag
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

openStatementP :: Parser Structure -> Parser (OpenDescription, ())
openStatementP structureP = do
  try $ openT
  override <- overrideFlagP
  -- (ext, attrs) <- extAttributesP
  i <- modLongidentP
  a <- postItemAttributesP structureP
  return (
    Opn.mk
    (def { override
         , attrs = a -- FIXME: ++ attrs
         , loc   = symbolRLoc ()
         , docs  = symbolDocs ()
         }
    )
    (mkRHS i 4)
    , () -- FIXME: ext
    )
