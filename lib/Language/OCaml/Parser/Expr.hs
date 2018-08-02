module Language.OCaml.Parser.Expr
  ( exprP
  ) where

import Text.Megaparsec                              hiding (token)

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
import Language.OCaml.PrettyPrinter                 ()

exprP :: Parser Structure -> Parser Expression -> Parser Expression
exprP structureP seqExprP = choice
  [ try $ parser <* notFollowedBy commaT
  , mkexp . PexpTuple . reverse <$>
    chainl1' parser (commaT *> (return $ flip (:))) (: [])
  ]
  where
    parser = leftRecursive
      [ try $ do
        e <- simpleExprP'
        l <- simpleLabeledExprListP'
        return $ mkexp $ PexpApply e (reverse l)
      , do
        b <- try $ do
          b <- letBindingsP structureP seqExprP
          inT
          return b
        e <- seqExprP
        return $ exprOfLetBindings b e
      -- FUNCTION
      , do
        try $ functionT
        -- TODO: extAttributes
        optBarP
        l <- matchCasesP'
        return $ mkexpAttrs (PexpFunction $ reverse l) (Nothing, []) -- FIXME
      -- FUN
      , do
        try $ funT
        -- TODO: extAttributes
        (l, o, p) <- labeledSimplePatternP patternP
        d <- funDefP seqExprP
        return $ mkexpAttrs (PexpFun l o p d) (Nothing, []) -- FIXME
      -- MATCH
      , do
        try $ matchT
        -- TODO: extAttributes
        e <- seqExprP
        withT
        optBarP
        l <- matchCasesP'
        return $ mkexpAttrs (PexpMatch e $ reverse l) (Nothing, []) -- FIXME
      -- IF ... THEN ... ELSE ...
      , do
        try ifT
        -- TODO: extAttributes
        c <- seqExprP
        thenT
        t <- exprP'
        e <- choice
          [ do
            try elseT
            Just <$> exprP'
          , return Nothing
          ]
        return $ mkexpAttrs (PexpIfThenElse c t e) (Nothing, []) -- FIXME
      , simpleExprP'
      ]
      -- FIXME: deal with associativity?
      [ infixP equalT   "="
      , infixP plusT    "+"
      , infixP minusT   "-"
      , infixP starT    "*"
      , infixP caretT   "^"
      , infixP greaterT ">"
      , infixP atT      "@"
      ]

    infixP :: Parser () -> String -> Parser (Expression -> Expression)
    infixP token symbol = try $ do
      token
      e2 <- exprP'
      return $ \ e1 -> mkinfix e1 symbol e2

    exprP'                     = exprP                     structureP seqExprP
    simpleExprP'              = simpleExprP                          seqExprP exprP'
    simpleLabeledExprListP' = simpleLabeledExprListP             seqExprP exprP'
    matchCasesP'              = matchCasesP                          seqExprP
