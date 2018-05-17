module Language.OCaml.PrettyPrinter.TestUtils
  ( DebugPrettyPrinter(..)
  , debugPrettyPrinter
  , mkPrettyPrinterTest
  , parseAndPrettyPrint
  , parseMaybe
  , pretty
  , roundtrip
  ) where

import Data.Text.Prettyprint.Doc
import Text.Megaparsec
import Text.Megaparsec.String
import Test.Tasty
import Test.Tasty.HUnit

mkPrettyPrinterTest :: (Eq a) => TestName -> Parser a -> (a -> Doc b) -> String -> TestTree
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

data DebugPrettyPrinter a b
  = NoParse1
  | NoParse2 a (Doc b)
  | ParseDiff a a
  | ParseEq a

instance (Show a) => Show (DebugPrettyPrinter a b) where
  show NoParse1 = "The input did not parse correctly"
  show (NoParse2 a d) =
    "The input parsed correctly, but its pretty-printed version did not parse\n"
    ++ show a ++ "\n"
    ++ show d ++ "\n"
  show (ParseDiff a b) =
    "The input parsed correctly, but its re-parse differs from it\n"
    ++ show a ++ "\n"
    ++ show b ++ "\n"
  show (ParseEq a) =
    "The input parsed correctly, and its re-parse matches\n"
    ++ show a ++ "\n"

debugPrettyPrinter ::
  (Eq a) => Parser a -> (a -> Doc b) -> String -> DebugPrettyPrinter a b
debugPrettyPrinter parser printer input =
  case parseMaybe parser input of
  Nothing -> NoParse1
  Just r ->
    case parseMaybe parser (show $ printer r) of
    Nothing -> NoParse2 r (printer r)
    Just r' -> if r == r' then ParseEq r' else ParseDiff r r'

parseAndPrettyPrint :: Parser a -> (a -> Doc b) -> String -> Maybe (Doc b)
parseAndPrettyPrint parser printer input = printer <$> parseMaybe parser input

roundtrip :: Parser a -> (a -> Doc b) -> String -> Maybe String
roundtrip p pp s = (show . pp) <$> parseMaybe p s
