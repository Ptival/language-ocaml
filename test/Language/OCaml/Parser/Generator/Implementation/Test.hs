{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Generator.Implementation.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

-- import Language.OCaml.Parser.Generator.Lexer
import Language.OCaml.Parser.Generator.Parser
import Language.OCaml.Parser.TestUtils
import Language.OCaml.Parser.Implementation.Test (testFiles)

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Implementation" $ []
  ++ map (mkParsingTestFromFileG parseImplementation) testFiles

test :: IO ()
test = defaultMain unitTests
