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

-- prefix :: FilePath
-- prefix = "test/ocaml/"

-- mlFiles :: [String]
-- mlFiles = []
--   ++ map (prefix ++)
--           [ "typing/typetexp.ml"
--           , "typing/path.ml"
--           , "typing/includecore.ml"
--           , "typing/printtyped.ml"
--           , "typing/typedecl.ml"
--           , "typing/btype.ml"
--           , "typing/typemod.ml"
--           , "typing/tast_mapper.ml"
--           , "typing/subst.ml"
--           , "typing/typecore.ml"
--           , "typing/datarepr.ml"
--           , "typing/typedtree.ml"
--           , "typing/untypeast.ml"
--           , "typing/parmatch.ml"
--           , "typing/ident.ml"
--           , "typing/cmt_format.ml"
--           , "typing/oprint.ml"
--           , "typing/env.ml"
--           , "typing/stypes.ml"
--           , "typing/includemod.ml"
--           , "typing/includeclass.ml"
--           , "typing/primitive.ml"
--           , "typing/envaux.ml"
--           , "typing/typeclass.ml"
--           , "typing/cmi_format.ml"
--           , "typing/printtyp.ml"
--           , "typing/mtype.ml"
--           , "typing/types.ml"
--           , "typing/typedtreeIter.ml"
--           , "typing/typedtreeMap.ml"
--           , "typing/typeopt.ml"
--           , "typing/printpat.ml"
--           , "typing/predef.ml"
--           , "typing/ctype.ml"
--           ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Generator.Implementation" $ []
  ++ map (mkParsingTestFromFileG parseImplementation) testFiles
  -- ++ map (mkParsingTestFromFileG parseImplementation) mlFiles

test :: IO ()
test = defaultMain unitTests
