{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.OCaml.Parser.LetBindingBody
  ( let_binding_body_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.FunBinding
import Language.OCaml.Parser.PatternNoExn
import Language.OCaml.Parser.SimplePatternNotIdent
import Language.OCaml.Parser.StrictBinding
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TypeConstraint
import Language.OCaml.Parser.ValIdent

let_binding_body_P :: Parser Expression -> Parser (Pattern, Expression)
let_binding_body_P seq_expr_P = choice
  [ try $ do
    i <- val_ident_P
    b <- strict_binding_P seq_expr_P (fun_binding_P seq_expr_P)
    return (mkpatvar i 1, b)
  , do
    (i, c) <- try $ do
      i <- val_ident_P
      c <- type_constraint_P
      return (i, c)
    equal_T
    e <- seq_expr_P
    let v = mkpatvar i 1
    let t = case c of
          (Just t', Nothing) -> t'
          (_,       Just t') -> t'
          _ -> error "This should not happen"
    return ( ghpat $ Ppat_constraint v (ghtyp $ Ptyp_poly [] t)
           , mkexp_constraint e c
           )
  -- TODO: typevar_list
  -- TODO: lident_list
  , do
    p <- try $ do
      p <- pattern_no_exn_P
      equal_T
      return p
    e <- seq_expr_P
    return (p, e)
  , do
    p <- try $ do
      p <- simple_pattern_not_ident_P
      colon_T
      return p
    t <- core_type_P
    equal_T
    e <- seq_expr_P
    return ( ghpat $ Ppat_constraint p t
           , e
           )
  ]
