{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Tokens.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
-- import Text.Megaparsec
import Test.Tasty

import Language.OCaml.Parser.Internal
import Language.OCaml.Parser.TestUtils

-- lIdentTests :: [String]
-- lIdentTests = [ "a", "a " ]

-- uIdentTests :: [String]
-- uIdentTests = [ "A", "A " ]

-- starTests :: [String]
-- starTests = [ "*", "* ", " *", " * " ]

stringTests :: [String]
stringTests =
  [ [s|"foo"|]
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Tokens" $ []
  ++ map (mkParsingTest "stringT" stringT) stringTests

test :: IO ()
test = defaultMain unitTests

-- foo = debugParsing (stringT *> stringT) [s|"foo" "bar"|]
