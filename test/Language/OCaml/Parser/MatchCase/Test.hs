{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser.MatchCase.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

matchCaseTests :: [String]
matchCaseTests =
  [ "a -> b"
  , "a -> B.c_d"
  , "a when b -> c"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.MatchCase" $ []
  ++ map (mkParsingTest "matchCaseP" matchCaseP) matchCaseTests

test :: IO ()
test = defaultMain unitTests
