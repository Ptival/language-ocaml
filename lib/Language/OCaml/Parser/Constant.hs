module Language.OCaml.Parser.Constant
  ( constant_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

constant_P :: Parser Constant
constant_P = choice
  [ do
    (n, m) <- int_T
    return $ Pconst_integer n m
  , Pconst_char <$> char_T
  , do
    (s, d) <- string_T
    return $ Pconst_string s d
  -- , do
  --   (f, m) <- string_T
  --   return $ Pconst_float f m
  ]
