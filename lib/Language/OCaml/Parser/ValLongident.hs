module Language.OCaml.Parser.ValLongident
  ( val_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent

val_longident_P :: Parser Longident
val_longident_P = choice
  [ Lident <$> val_ident_P
  , do
    l <- try $ do
      l <- mod_longident_P
      dot_T
      return l
    i <- val_ident_P
    return $ Ldot l i
  ]
