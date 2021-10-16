module Language.OCaml.PrettyPrinter.TestUtils
  ( DebugPrettyPrinter (..),
    debugPrettyPrinter,
    mkPrettyPrinterTest,
    parseAndPrettyPrint,
    parseMaybe,
    pretty,
    roundtrip,
  )
where

-- import Language.OCaml.

import Language.OCaml.Parser
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

mkPrettyPrinterTest :: (Eq a) => TestName -> Parser a -> (a -> Doc b) -> String -> TestTree
mkPrettyPrinterTest name parser printer input =
  testCase name $ condition @? message
  where
    condition =
      case parser input of
        Left _ -> False
        Right r ->
          case parser (show $ printer r) of
            Left _ -> False
            Right r' -> r == r'
    message = "Failed to roundtrip:\n" ++ prefix ++ if length input > 20 then "..." else ""
    prefix = take 20 input

data DebugPrettyPrinter a b
  = NoParse1
  | NoParse2 a (Doc b)
  | ParseDiff a a
  | ParseEq a

instance (Pretty a, Show a) => Show (DebugPrettyPrinter a b) where
  show NoParse1 = "The input did not parse correctly"
  show (NoParse2 a d) =
    "The input parsed correctly, but its pretty-printed version did not parse\n"
      ++ show a
      ++ "\n"
      ++ show d
      ++ "\n"
  show (ParseDiff a b) =
    "The input parsed correctly, but its re-parse differs from it\n"
      ++ show a
      ++ "\n"
      ++ show b
      ++ "\n"
      ++ show (pretty a)
      ++ "\n"
      ++ show (pretty b)
      ++ "\n"
  show (ParseEq a) =
    "The input parsed correctly, and its re-parse matches\n"
      ++ show a
      ++ "\n"
      ++ show (pretty a)
      ++ "\n"

debugPrettyPrinter ::
  (Eq a) => Parser a -> (a -> Doc b) -> String -> DebugPrettyPrinter a b
debugPrettyPrinter parser printer input =
  case parser input of
    Left _ -> NoParse1
    Right r ->
      case parser (show $ printer r) of
        Left _ -> NoParse2 r (printer r)
        Right r' -> if r == r' then ParseEq r' else ParseDiff r r'

parseAndPrettyPrint :: Parser a -> (a -> Doc b) -> String -> Either String (Doc b)
parseAndPrettyPrint parser printer input = printer <$> parser input

roundtrip :: Parser a -> (a -> Doc b) -> String -> Either String String
roundtrip p pp s = (show . pp) <$> p s
