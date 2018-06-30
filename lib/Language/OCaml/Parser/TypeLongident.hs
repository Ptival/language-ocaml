module Language.OCaml.Parser.TypeLongident
  ( typeLongidentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModExtLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

typeLongidentP :: Parser Longident
typeLongidentP = choice
  [ Lident <$> lIdentT
  , do
    p <- modExtLongidentP
    dotT
    i <- lIdentT
    return $ Ldot p i
  ]
