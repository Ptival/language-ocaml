{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.OpenStatement
  ( open_statement_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Override_flag
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

open_statement_P :: Parser Structure -> Parser (Open_description, ())
open_statement_P structure_P = do
  try $ open_T
  o <- override_flag_P
  -- TODO: ext_attributes
  i <- mod_longident_P
  a <- post_item_attributes_P structure_P
  return $ (
    mkOpn
    Nothing -- FIXME
    Nothing -- FIXME
    Nothing -- FIXME
    (Just o)
    (mkRHS i 4)
    , ())
