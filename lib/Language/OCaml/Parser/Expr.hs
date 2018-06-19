module Language.OCaml.Parser.Expr
  ( expr_P
  ) where

import Text.Megaparsec hiding (token)

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.FunDef
import Language.OCaml.Parser.LabeledSimplePattern
import Language.OCaml.Parser.LetBindings
import Language.OCaml.Parser.MatchCases
import Language.OCaml.Parser.OptBar
import Language.OCaml.Parser.Pattern
import Language.OCaml.Parser.SimpleLabeledExprList
import Language.OCaml.Parser.SimpleExpr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types
import Language.OCaml.PrettyPrinter ()

expr_P :: Parser Structure -> Parser Expression -> Parser Expression
expr_P structure_P seq_expr_P = choice
  [ try $ parser <* notFollowedBy comma_T
  , mkexp . Pexp_tuple . reverse <$>
    chainl1' parser (comma_T *> (return $ flip (:))) (: [])
  ]
  where
    parser = leftRecursive
      [ try $ do
        e <- simple_expr_P'
        l <- simple_labeled_expr_list_P seq_expr_P
        return $ mkexp $ Pexp_apply e (reverse l)
      , do
        b <- try $ do
          b <- let_bindings_P structure_P seq_expr_P
          in_T
          return b
        e <- seq_expr_P
        return $ expr_of_let_bindings b e
      -- FUNCTION
      , do
        try $ function_T
        -- TODO: ext_attributes
        opt_bar_P
        l <- match_cases_P'
        return $ mkexp_attrs (Pexp_function $ reverse l) (Nothing, []) -- FIXME
      -- FUN
      , do
        try $ fun_T
        -- TODO: ext_attributes
        (l, o, p) <- labeled_simple_pattern_P pattern_P
        d <- fun_def_P seq_expr_P
        return $ mkexp_attrs (Pexp_fun l o p d) (Nothing, []) -- FIXME
      -- MATCH
      , do
        try $ match_T
        -- TODO: ext_attributes
        e <- seq_expr_P
        with_T
        opt_bar_P
        l <- match_cases_P'
        return $ mkexp_attrs (Pexp_match e $ reverse l) (Nothing, []) -- FIXME
      -- IF ... THEN ... ELSE ...
      , do
        try if_T
        -- TODO: ext_attributes
        c <- seq_expr_P
        then_T
        t <- expr_P'
        e <- choice
          [ do
            try else_T
            Just <$> expr_P'
          , return Nothing
          ]
        return $ mkexp_attrs (Pexp_ifthenelse c t e) (Nothing, []) -- FIXME
      , simple_expr_P'
      ]
      [ infixP equal_T   "="
      , infixP plus_T    "+"
      , infixP minus_T   "-"
      , infixP caret_T   "^"
      , infixP greater_T ">"
      , infixP at_T      "@"
      ]

    infixP :: Parser () -> String -> Parser (Expression -> Expression)
    infixP token symbol = try $ do
      token
      e2 <- expr_P'
      return $ \ e1 -> mkinfix e1 symbol e2

    expr_P'        = expr_P        structure_P seq_expr_P
    simple_expr_P' = simple_expr_P seq_expr_P
    match_cases_P' = match_cases_P seq_expr_P
