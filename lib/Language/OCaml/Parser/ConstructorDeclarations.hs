{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.ConstructorDeclarations
  ( constructor_declarations_P
  ) where

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.BarConstructorDeclaration
import Language.OCaml.Parser.ConstructorDeclaration
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Combinators
import Language.OCaml.Parser.Utils.Types

constructor_declarations_P :: Parser [Constructor_declaration]
constructor_declarations_P = leftRecursive
  [ (: []) <$> constructor_declaration_P
  , (: []) <$> bar_constructor_declaration_P
  , bar_T *> return []
  ]
  [ (:) <$> bar_constructor_declaration_P
  ]
