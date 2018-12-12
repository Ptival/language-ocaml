module Language.OCaml.Parser.Constant.Test
  ( testStrings
  ) where

import qualified Language.OCaml.Parser.Int.Test as Int

testStrings :: [String]
testStrings = []
  ++ Int.testStrings
  -- TODO: CHAR
  -- TODO: STRING
  -- TODO: FLOAT
