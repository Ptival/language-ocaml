{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.Expr
  ( expr_P
  ) where

import Data.Text.Prettyprint.Doc
import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LetBindings
import Language.OCaml.Parser.MatchCases
import Language.OCaml.Parser.OptBar
import Language.OCaml.Parser.SimpleLabeledExprList
import Language.OCaml.Parser.SimpleExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.PrettyPrinter ()

expr_P :: Parser Structure -> Parser Expression -> Parser Expression
expr_P structure_P seq_expr_P = choice
  [ try $ p <* notFollowedBy comma_T
  , mkexp . Pexp_tuple . reverse <$>
    chainl1' p (comma_T *> (return $ flip (:))) (: [])
  ]
  where
    p = choice
      [ try $ do
        e <- simple_expr_P'
        l <- simple_labeled_expr_list_P seq_expr_P
        return $ mkexp $ Pexp_apply e (checkReverse "B" l)
      , do
        b <- try $ do
          b <- let_bindings_P structure_P seq_expr_P
          in_T
          return b
        e <- seq_expr_P
        return $ expr_of_let_bindings b e
      , do
        try $ function_T
        -- TODO: ext_attributes
        opt_bar_P
        l <- match_cases_P seq_expr_P
        return $ mkexp_attrs (Pexp_function $ reverse l) (Nothing, []) -- FIXME
      , simple_expr_P'
      ]

    simple_expr_P' = simple_expr_P seq_expr_P

    checkReverse _ [] = []
    checkReverse _ [e] = [e] -- if there's just one element, it's not going to help...
    checkReverse s l =
      error $ s ++ ": figure out whether to reverse this list:\n" ++ (show $ pretty l)
