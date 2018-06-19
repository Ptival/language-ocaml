module Language.OCaml.Parser.PatternGen
  ( pattern_gen_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.ConstrLongident
import Language.OCaml.Parser.SimplePattern
import Language.OCaml.Parser.Utils.Types

pattern_gen_P :: Parser Pattern -> Parser Pattern
pattern_gen_P pattern_P = choice
  [ try $ do
    i <- constr_longident_P
    p <- pattern_P
    return $ mkpat $ Ppat_construct (mkRHS i 1) (Just p)
  , try $ simple_pattern_P pattern_P
  ]
