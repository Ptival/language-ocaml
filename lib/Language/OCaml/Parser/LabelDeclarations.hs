{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.OCaml.Parser.LabelDeclarations
  ( label_declarations_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.LabelDeclaration
import Language.OCaml.Parser.LabelDeclarationSemi

label_declarations_P :: Parser [Label_declaration]
label_declarations_P = choice
  [ do
    h <- label_declaration_semi_P
    t <- label_declarations_P
    return $ h : t
  , (: []) <$> label_declaration_semi_P
  , (: []) <$> label_declaration_P
  ]
