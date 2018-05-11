module Language.OCaml.PrettyPrinter.TestUtils
  ( DebugPrettyPrinter(..)
  , debugPrettyPrinter
  , mkPrettyPrinterTest
  , parseAndPrettyPrint
  ) where

import Data.Text.Prettyprint.Doc
import Text.Megaparsec
import Text.Megaparsec.String
import Test.Tasty
import Test.Tasty.HUnit

mkPrettyPrinterTest :: (Eq a) => TestName -> Parser a -> (a -> Doc a) -> String -> TestTree
mkPrettyPrinterTest name parser printer input =
  testCase name $ condition @? message
  where
    condition =
      case parseMaybe parser input of
      Nothing -> False
      Just r ->
        case parseMaybe parser (show $ printer r) of
        Nothing -> False
        Just r' -> r == r'
    message = "Failed to roundtrip:\n" ++ prefix ++ if length input > 20 then "..." else ""
    prefix = take 20 input

data DebugPrettyPrinter a
  = NoParse1
  | NoParse2 a (Doc a)
  | Parse a a
  deriving (Show)

debugPrettyPrinter ::
  (Eq a) => Parser a -> (a -> Doc a) -> String -> DebugPrettyPrinter a
debugPrettyPrinter parser printer input =
  case parseMaybe parser input of
  Nothing -> NoParse1
  Just r ->
    case parseMaybe parser (show $ printer r) of
    Nothing -> NoParse2 r (printer r)
    Just r' -> Parse r r'

parseAndPrettyPrint :: Parser a -> (a -> Doc a) -> String -> Maybe (Doc a)
parseAndPrettyPrint parser printer input = printer <$> parseMaybe parser input
