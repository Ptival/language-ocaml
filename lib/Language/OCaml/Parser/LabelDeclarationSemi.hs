module Language.OCaml.Parser.LabelDeclarationSemi
  ( label_declaration_semi_P
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Label
import Language.OCaml.Parser.MutableFlag
import Language.OCaml.Parser.PolyTypeNoAttr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

label_declaration_semi_P :: Parser Label_declaration
label_declaration_semi_P = try $ do
  mut <- mutable_flag_P
  label <- label_P
  colon_T
  t <- poly_type_no_attr_P
  -- TODO: attributes
  semi_T
  -- TODO: attributes
  return $ field mut (mkRHS label 2) t
