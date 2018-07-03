module Language.OCaml.Parser.TestUtils
  ( debugParsing
  , mkParsingTest
  , mkParsingTestG
  , mkParsingTestFromFile
  , mkParsingTestFromFileG
  , parse
  ) where

import Data.Either
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.Utils.Utils

mkParsingTest :: TestName -> Parser a -> String -> TestTree
mkParsingTest name parser input =
  let prefix = take 20 input in
  testCase name
  $ isJust (parseMaybe parser input)
  @? "Failed to parse:\n" ++ prefix ++ if length input > 20 then "..." else ""

mkParsingTestG :: TestName -> (String -> Either String a) -> String -> TestTree
mkParsingTestG name parser input =
  let prefix = take 20 input in
  testCase name
  $ isRight (parser input)
  @? "Failed to parse:\n" ++ prefix ++ if length input > 20 then "..." else ""

mkParsingTestFromFile :: Parser a -> FilePath -> TestTree
mkParsingTestFromFile parser fileName =
  testCase fileName
    $ (do
        input <- readFile fileName
        return $ isJust (parseMaybe parser input)
      )
    @? "Failed to parse " ++ fileName

mkParsingTestFromFileG :: (String -> Either String a) -> FilePath -> TestTree
mkParsingTestFromFileG parser fileName =
  testCase fileName
    $ (do
        input <- readFile fileName
        return $ isRight (parser input)
      )
    @? "Failed to parse " ++ fileName

debugParsing :: Parser a -> String -> Either (ParseError (Token String) Void) a
debugParsing parser input =
  parse (ocamlSpace *> parser <* eof) "DEBUG" input
