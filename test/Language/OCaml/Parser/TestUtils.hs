module Language.OCaml.Parser.TestUtils
  ( compareParses
  , debugParsing
  , mkParsingTest
  , mkParsingTestG
  , mkParsingTestFromFile
  , mkParsingTestFromFileG
  , parse
  ) where

import Data.Void
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit

import Language.OCaml.Parser
import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.Utils.Utils

assertParsesC :: ParserC a -> String -> Assertion
assertParsesC parser input = do
  case parseMaybe parser input of
    Nothing -> assertFailure $ "Failed to parse: " ++ take 60 input
    Just _  -> return ()

assertParsesG :: ParserG a -> String -> Assertion
assertParsesG parser input = do
  case parser input of
    Left msg -> assertFailure msg
    Right _  -> return ()

mkParsingTest :: TestName -> Parser a -> String -> TestTree
mkParsingTest name parser input = testCase name $ assertParsesC parser input

mkParsingTestG :: (String -> Either String a) -> String -> TestTree
mkParsingTestG parser input = testCase shortInput $ assertParsesG parser input
  where
    shortInput | length input <= 40 = input
               | otherwise          = take 40 input ++ "..."

mkParsingTestFromFile :: Parser a -> FilePath -> TestTree
mkParsingTestFromFile parser fileName = testCase fileName $ readFile fileName >>= assertParsesC parser

mkParsingTestFromFileG :: (String -> Either String a) -> FilePath -> TestTree
mkParsingTestFromFileG parser fileName = testCase fileName $ readFile fileName >>= assertParsesG parser

debugParsing :: Parser a -> String -> Either (ParseError (Token String) Void) a
debugParsing parser input =
  parse (ocamlSpace *> parser <* eof) "DEBUG" input

compareParses ::
  (Eq a, Show a) =>
  TestName -> Parser a -> (String -> Either String a) -> String -> TestTree
compareParses name parserC parserG input =
  testCase name $ do
  case (parseMaybe parserC input, parserG input) of
    (Just outputC, Right outputG) -> (outputC == outputG)
                                     @? ("Results differ:\n"
                                         ++ "Combinator: " ++ show outputC ++ "\n"
                                         ++ "Generator:  " ++ show outputG ++ "\n"
                                        )
    (Just _,       Left _)        -> False @? "Parser generator failed"
    (Nothing,      Right _)       -> False @? "Parser combinator failed"
    (Nothing,      Left _)        -> False @? "Both parsers failed"
