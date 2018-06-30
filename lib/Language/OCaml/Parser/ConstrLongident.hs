module Language.OCaml.Parser.ConstrLongident
  ( constrLongidentP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ModLongident
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.Parser.Utils.Utils

constrLongidentP :: Parser Longident
constrLongidentP = choice
  [ do
    i <- try $ do
      i <- modLongidentP
      dotT
      return i
    parens colonColonT
    return $ Ldot i "::"
  , modLongidentP
  , try $ lBracketT *> rBracketT *> (return $ Lident "[]")
  , try $ lParenT *> rParenT *> (return $ Lident "()")
  , try (lParenT *> colonColonT) *> rParenT *> (return $ Lident "::")
  , falseT *> (return $ Lident "false")
  , trueT *> (return $ Lident "true")
  ]
