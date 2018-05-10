{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ConstructorDeclarations
  ( constructor_declarations_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.ConstructorDeclaration
import Language.OCaml.Parser.Tokens

constructor_declarations_P :: Parser [Constructor_declaration]
constructor_declarations_P = choice
  [ constructor_declaration_P `sepBy1` bar_T
  , try (bar_T *> constructor_declaration_P `sepBy1` bar_T)
  , (: []) <$> (try $ bar_T *> constructor_declaration_P)
  , bar_T *> return []
  ]
