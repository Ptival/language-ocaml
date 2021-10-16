module Language.OCaml.Parser.TestUtils
  ( mkParsingTest,
    mkParsingTestFromFile,
    parse,
  )
where

import Language.OCaml.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

assertParses :: Parser a -> String -> Assertion
assertParses parser input = do
  case parser input of
    Left msg -> assertFailure msg
    Right _ -> return ()

mkParsingTest :: (String -> Either String a) -> String -> TestTree
mkParsingTest parser input = testCase shortInput $ assertParses parser input
  where
    shortInput
      | length input <= 40 = input
      | otherwise = take 40 input ++ "..."

mkParsingTestFromFile :: (String -> Either String a) -> FilePath -> TestTree
mkParsingTestFromFile parser fileName =
  testCase fileName $ readFile fileName >>= assertParses parser
