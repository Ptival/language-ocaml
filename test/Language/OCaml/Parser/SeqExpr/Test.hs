{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.SeqExpr.Test
  ( testStrings,
  )
where

import Data.String.Interpolate
import qualified Language.OCaml.Parser.Expr.Test as Expr

limit :: Int
limit = 5

testStrings :: [String]
testStrings =
  []
    ++ expr
    ++ [[i|#{e};|] | e <- expr]
    ++ [ [i|#{e};#{se}|] | e <- expr, se <- seqExpr
       ]
  where
    expr = Expr.testStrings seqExpr
    seqExpr = take limit testStrings
