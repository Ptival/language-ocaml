module Language.OCaml.Parser.SingleAttrId
  ( singleAttrIdP
  ) where

import Text.Megaparsec

import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

singleAttrIdP :: Parser String
singleAttrIdP = choice
  [ lIdentT
  , uIdentT
  -- TODO
  ]
