module Language.OCaml.Parser.LetBindingBody
  ( letBindingBodyP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.FunBinding
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.PatternNoExn
import Language.OCaml.Parser.SimplePatternNotIdent
import Language.OCaml.Parser.StrictBinding
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.ValIdent
import Language.OCaml.Parser.Utils.Types

letBindingBodyP :: Parser Expression -> Parser (Pattern, Expression)
letBindingBodyP seqExprP = choice
  [ try $ do
    i <- valIdentP
    b <- strictBindingP seqExprP (funBindingP seqExprP)
    return (mkpatvar i 1, b)
  , do
    (i, c) <- try $ do
      i <- valIdentP
      c <- typeConstraintP
      return (i, c)
    equalT
    e <- seqExprP
    let v = mkpatvar i 1
    let t = case c of
          (Just t', Nothing) -> t'
          (_,       Just t') -> t'
          _ -> error "This should not happen"
    return ( ghpat $ PpatConstraint v (ghtyp $ PtypPoly [] t)
           , mkexpConstraint e c
           )
  -- TODO: typevarList
  -- TODO: lidentList
  , do
    p <- try $ do
      p <- patternNoExnP
      equalT
      return p
    e <- seqExprP
    return (p, e)
  , do
    p <- try $ do
      p <- simplePatternNotIdentP patternP
      colonT
      return p
    t <- coreTypeP
    equalT
    e <- seqExprP
    return ( ghpat $ PpatConstraint p t
           , e
           )
  ]
