module Language.OCaml.Parser.LetBinding
  ( letBindingP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LetBindingBody
import Language.OCaml.Parser.PostItemAttributes
import Language.OCaml.Parser.RecFlag
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

letBindingP :: Parser Structure -> Parser Expression -> Parser LetBindings
letBindingP structureP seqExprP = do
  try letT
  -- TODO: extAttributes
  r <- recFlagP
  b <- letBindingBodyP seqExprP
  _a <- postItemAttributesP structureP
  return $ mklbs Nothing r (mklb True b []) -- FIXME
