module Language.OCaml.Parser.ModuleBinding
  ( moduleBindingP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ModuleBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

moduleBindingP :: Parser Structure -> Parser (ModuleBinding, ())
moduleBindingP structureP = do
  try $ moduleT
  -- TODO: extAttributes
  i <- uIdentT
  b <- moduleBindingBodyP
  _a <- postItemAttributesP structureP
  return $ (mkMb Nothing Nothing Nothing Nothing (mkRHS i 3) b -- FIXME
           , () -- FIXME
           )
