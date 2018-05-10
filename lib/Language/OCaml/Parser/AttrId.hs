module Language.OCaml.Parser.AttrId
  ( attr_id_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.SingleAttrId
import           Language.OCaml.Parser.Tokens

attr_id_P :: Parser (ASTTypes.Loc String)
attr_id_P = choice
  [ do
    a <- try $ single_attr_id_P <* dot_T
    b <- attr_id_P
    return $ mkLoc (a ++ "^" ++ ASTTypes.txt b) none -- FIXME symbol_rloc
  , single_attr_id_P >>= \ a -> return $ mkLoc a none -- FIXME symbol_rloc
  ]
