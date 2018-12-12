module Language.OCaml.Parser.OpenStatement.Test
  ( testStrings
  ) where

testStrings :: [String]
testStrings =
  [ "open A"
  , "open !A"
  -- , "(* A *) open !A"
  , "open !A (* A *)"
  -- , "(* A *) open !A (* A *)"
  ]
