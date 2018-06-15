{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.TypeKind
  ( type_kind_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorDeclarations
import Language.OCaml.Parser.CoreType
import Language.OCaml.Parser.LabelDeclarations
import Language.OCaml.Parser.PrivateFlag
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

type_kind_P :: Parser Structure -> Parser (Type_kind, Private_flag, Maybe Core_type)
type_kind_P structure_P = choice
  [ do
    t <- try $ do
      equal_T
      core_type_P
    return (Ptype_abstract, Public, Just t)
  , do
    cs <- try $ do
      equal_T
      constructor_declarations_P'
    return (Ptype_variant (reverse cs), Private, Nothing)
  , do
    priv <- try $ do
      equal_T
      priv <- private_flag_P
      l_brace_T
      return priv
    labels <- label_declarations_P
    r_brace_T
    return (Ptype_record labels, priv, Nothing)
  , return (Ptype_abstract, Public, Nothing)
  ]
  where
    constructor_declarations_P' = constructor_declarations_P structure_P core_type_P
