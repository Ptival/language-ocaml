{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.StrictBinding.Test
  ( testStrings
  ) where

import Data.String.Interpolate

import qualified Language.OCaml.Parser.SeqExpr.Test as SeqExpr

limit :: Int
limit = 100

testStrings :: [String]
testStrings = []
  ++ [ [i| = #{se} |]
       | se <- seqExpr
       ]
  -- TODO: rest
  where
    seqExpr = take limit $ SeqExpr.testStrings
