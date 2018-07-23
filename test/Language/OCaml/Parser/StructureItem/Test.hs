{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.StructureItem.Test
  ( testStrings
  ) where

import qualified Language.OCaml.Parser.LetBindings.Test as LetBindings

limit :: Int
limit = 10

testStrings :: [String] -> [String]
testStrings structure = []
  ++ letBindings
  where
    letBindings = take limit $ LetBindings.testStrings structure
