module Language.OCaml.Parser.ModExtLongident
  ( modExtLongidentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

modExtLongidentP :: Parser Longident
modExtLongidentP = leftRecursive
  [ Lident <$> uIdentT
  ]
  [ try $ do
    dotT
    i <- uIdentT
    return $ \ x -> Ldot x i
    -- TODO: parens
  ]
