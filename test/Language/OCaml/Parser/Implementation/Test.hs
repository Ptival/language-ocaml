{-# LANGUAGE QuasiQuotes #-}

module Language.OCaml.Parser.Implementation.Test
  ( test
  , unitTests
  ) where

-- import Data.String.QQ
import Test.Tasty

import Language.OCaml.Parser.Internal
-- import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.TestUtils

prefix :: FilePath
prefix = "test/Language/OCaml/Parser/Implementation/"

files :: [FilePath]
files = map (prefix ++)
  [ "test_00.ml"
  , "test_01.ml"
  , "test_02.ml"
  , "test_03.ml"
  , "test_04.ml"
  , "test_05.ml"
  , "test_06.ml"
  , "test_07.ml"
  , "test_08.ml"
  -- , "FaCT/tast.ml"
  -- , "FaCT/tast-00.ml"
  -- , "FaCT/tast-01.ml"
  ]

unitTests :: TestTree
unitTests = testGroup "Language.OCaml.Parser.Implementation" $ []
  ++ map (mkParsingTestFromFile implementation_P) files

test :: IO ()
test = defaultMain unitTests

-- foo = debugParsing implementation_P <$> readFile (prefix ++ "FaCT/tast-01.ml")

-- foo = debugParsing implementation_P <$> readFile (prefix ++ "stdlib/arg.ml")

-- bar = debugParsing implementation_P [s|

-- let update_fn venv { pos=p; data=tfdec } =
--   let args = List.map (fun { data={ lt } } -> lt) tfdec.t_params in
--   Hashtbl.add venv tfdec.t_name (Env.FunEntry { f_rty=tfdec.t_rty; f_rlbl=tfdec.t_rlbl; f_args=args })

--   |]

-- baz = debugParsing record_expr_P "a = b"
