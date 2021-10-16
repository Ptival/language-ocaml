module Language.OCaml.Parser.Pattern.Test
  ( testStrings,
  )
where

testStrings :: [String]
testStrings =
  [ "_",
    "a",
    "A",
    "A a",
    "A B",
    "A _",
    "A _ | B _",
    "_, _",
    "A _, B _"
  ]
