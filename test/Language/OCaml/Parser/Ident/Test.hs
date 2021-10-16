module Language.OCaml.Parser.Ident.Test
  ( testStrings,
  )
where

testStrings :: [String]
testStrings =
  [ "A",
    "Ab",
    "AB",
    "a",
    "aB",
    "ab",
    "A ",
    "Ab ",
    "AB ",
    "a ",
    "aB ",
    "ab "
  ]
