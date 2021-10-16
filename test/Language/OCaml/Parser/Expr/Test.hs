module Language.OCaml.Parser.Expr.Test
  ( testStrings,
  )
where

import qualified Language.OCaml.Parser.SimpleExpr.Test as SimpleExpr

testStrings :: [String] -> [String]
testStrings seqExpr =
  []
    ++ SimpleExpr.testStrings seqExpr
