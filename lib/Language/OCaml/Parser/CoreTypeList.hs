{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.CoreTypeList
  ( core_type_list_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.SimpleCoreType
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

core_type_list_P :: Parser [Core_type]
core_type_list_P = leftRecursive
  [ (: []) <$> simple_core_type_P
  ]
  [ do
    try $ star_T
    t <- simple_core_type_P
    return $ (:) t
  ]
