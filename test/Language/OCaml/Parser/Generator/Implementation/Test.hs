{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.Implementation.Test
  ( test
  , mkUnitTest
  ) where

import Test.Tasty

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.Implementation.Test

mkUnitTest :: [FilePath] -> TestTree
mkUnitTest fileNames =
  testGroup "Language.OCaml.Parser.Generator.Implementation" $ []
    ++ map testMLFile fileNames

test :: IO ()
test = do
  mlFiles <- listMLFiles
  defaultMain $ mkUnitTest mlFiles

debug :: Either String [StructureItem]
debug = parseImplementation "let f x : b = c"
