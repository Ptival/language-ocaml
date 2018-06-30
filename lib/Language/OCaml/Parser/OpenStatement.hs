module Language.OCaml.Parser.OpenStatement
  ( openStatementP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.OverrideFlag
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

openStatementP :: Parser Structure -> Parser (OpenDescription, ())
openStatementP structureP = do
  try $ openT
  o <- overrideFlagP
  -- TODO: extAttributes
  i <- modLongidentP
  _a <- postItemAttributesP structureP
  return $ (
    mkOpn
    Nothing -- FIXME
    Nothing -- FIXME
    Nothing -- FIXME
    (Just o)
    (mkRHS i 4)
    , ())
