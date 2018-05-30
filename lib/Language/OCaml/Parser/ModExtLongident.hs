module Language.OCaml.Parser.ModExtLongident
  ( mod_ext_longident_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

mod_ext_longident_P :: Parser Longident
mod_ext_longident_P = leftRecursive
  [ Lident <$> u_ident_T
  ]
  [ try $ do
    dot_T
    i <- u_ident_T
    return $ \ x -> Ldot x i
    -- TODO: parens
  ]
