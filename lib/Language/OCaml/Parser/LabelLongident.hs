module Language.OCaml.Parser.LabelLongident
  ( label_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Tokens

label_longident_P :: Parser Longident
label_longident_P = choice
  [ Lident <$> l_ident_T
  , do
    m <- mod_longident_P
    dot_T
    i <- l_ident_T
    return $ Ldot m i
  ]
