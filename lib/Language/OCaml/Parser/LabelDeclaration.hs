module Language.OCaml.Parser.LabelDeclaration
  ( labelDeclarationP
  ) where

import Data.Default
import Text.Megaparsec hiding (label)

import Language.OCaml.Definitions.Parsing.ASTHelper.Type as Type
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Label
import Language.OCaml.Parser.MutableFlag
import Language.OCaml.Parser.PolyTypeNoAttr
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Types

labelDeclarationP :: Parser LabelDeclaration
labelDeclarationP = try $ do
  _mut <- mutableFlagP
  label <- labelP
  colonT
  t <- polyTypeNoAttrP
  -- TODO: attributes
  return $ field def (mkRHS label 2) t -- FIXME: def
