module Language.OCaml.Parser.Constant
  ( constantP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

constantP :: Parser Constant
constantP = choice
  [ do
    (n, m) <- intT
    return $ PconstInteger n m
  , PconstChar <$> charT
  , do
    (s, d) <- stringT
    return $ PconstString s d
  -- , do
  --   (f, m) <- stringT
  --   return $ PconstFloat f m
  ]
