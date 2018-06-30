module Language.OCaml.Parser.ValLongident
  ( valLongidentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.ValIdent

valLongidentP :: Parser Longident
valLongidentP = choice
  [ Lident <$> valIdentP
  , do
    l <- try $ do
      l <- modLongidentP
      dotT
      return l
    i <- valIdentP
    return $ Ldot l i
  ]
