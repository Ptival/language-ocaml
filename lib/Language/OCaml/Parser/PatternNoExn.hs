module Language.OCaml.Parser.PatternNoExn
  ( patternNoExnP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.PatternGen
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

patternNoExnP :: Parser Pattern
patternNoExnP = leftRecursive
  [ patternGenP patternP
  ]
  [ do
    try asT
    i <- valIdentP
    return $ \ x -> mkpat $ PpatAlias x (mkRHS i 3)
  -- TODO: patternNoExnCommaList
  -- , do
  --   try colonColonT
  --   p <- patternP
  --   return $ mkpatCons _ _
  , do
    try barT
    p <- patternP
    return $ \ x -> mkpat $ PpatOr x p
  -- TODO: attribute
  ]
