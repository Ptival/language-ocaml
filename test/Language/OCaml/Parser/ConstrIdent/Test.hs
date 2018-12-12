module Language.OCaml.Parser.ConstrIdent.Test
  ( testStrings
  ) where

testStrings :: [String]
testStrings =
  [ "A"
  , "Ab"
  , "AB"
  , "[]"
  , "false"
  , "true"
  , "A "
  , "Ab "
  , "AB "
  , "[] "
  , "false "
  , "true "
  ]
