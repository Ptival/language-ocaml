{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( debugLexer
  , listMLFiles
  , testMLFile
  , testStrings
  ) where

import           Test.Tasty
import           Test.Tasty.Golden

import           Language.OCaml.Parser.Generator.Lexer
import           Language.OCaml.Parser.Generator.Parser
import qualified Language.OCaml.Parser.Structure.Test         as Structure
import           Language.OCaml.Parser.TestUtils

directory :: FilePath
directory = "test/Language/OCaml/Parser/Implementation"

testStrings :: [String]
testStrings = Structure.testStrings

listMLFiles :: IO [FilePath]
listMLFiles = findByExtension [".ml"] directory

testMLFile :: FilePath -> TestTree
testMLFile fileName = mkParsingTestFromFile parseImplementation fileName

debugLexer :: String -> Either String [ResultToken]
debugLexer = alexScanTokens
