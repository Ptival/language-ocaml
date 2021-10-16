module Language.OCaml.Parser.MatchCase.Test
  ( testStrings,
  )
where

testStrings :: [String]
testStrings =
  [ "a -> b",
    "a -> B.c_d",
    "a when b -> c"
  ]
