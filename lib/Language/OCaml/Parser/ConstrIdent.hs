module Language.OCaml.Parser.ConstrIdent
  ( constr_ident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens

constr_ident_P :: Parser String
constr_ident_P = choice
  [ u_ident_T
  , l_bracket_T *> r_bracket_T *> return "[]"
  , l_paren_T *> r_paren_T *> return "()"
  , l_paren_T *> colon_colon_T *> r_paren_T *> return "::"
  , false_T *> return "false"
  , true_T *> return "true"
  ]
