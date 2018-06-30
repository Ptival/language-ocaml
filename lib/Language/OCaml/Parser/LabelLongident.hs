module Language.OCaml.Parser.LabelLongident
  ( labelLongidentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

labelLongidentP :: Parser Longident
labelLongidentP = choice
  [ Lident <$> lIdentT
  , do
    m <- modLongidentP
    dotT
    i <- lIdentT
    return $ Ldot m i
  ]
