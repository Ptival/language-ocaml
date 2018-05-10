module Language.OCaml.Parser.SingleAttrId
  ( single_attr_id_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Parser.Tokens

single_attr_id_P :: Parser String
single_attr_id_P = choice
  [ l_ident_T
  , u_ident_T
  -- TODO
  ]
