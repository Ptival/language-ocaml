module Language.OCaml.Parser.AttrId
  ( attrIdP
  ) where

import           Text.Megaparsec

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.SingleAttrId
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Types

attrIdP :: Parser (ASTTypes.Loc String)
attrIdP = choice
  [ do
    a <- try $ singleAttrIdP <* dotT
    b <- attrIdP
    return $ mkLoc (a ++ "^" ++ ASTTypes.txt b) none -- FIXME symbolRLoc
  , singleAttrIdP >>= \ a -> return $ mkLoc a none -- FIXME symbolRLoc
  ]
