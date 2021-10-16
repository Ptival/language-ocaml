{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.Operator.Test
  ( testStrings,
  )
where

testStrings :: [String]
testStrings =
  [ "!",
    "+",
    "+.",
    "-",
    "-.",
    " *", -- needs space so that, when placed in parenthesis, does not become a comment
    "="
    -- TODO
  ]
