module Language.OCaml.Parser.TypeDeclarations
  ( type_declarations_P
  ) where

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AndTypeDeclaration
import           Language.OCaml.Parser.TypeDeclaration
import           Language.OCaml.Parser.Utils.Combinators
import           Language.OCaml.Parser.Utils.Types

type_declarations_P ::
  Parser Structure -> Parser (ASTTypes.Rec_flag, [Type_declaration])
type_declarations_P structure_P = leftRecursive
  [ do
    (nonrec_flag, ty) <- type_declaration_P'
    return $ (nonrec_flag, [ty])
  ]
  [ do
    ty <- and_type_declaration_P'
    return $ \ (nonrec_flag, tys) -> (nonrec_flag, ty : tys)
  ]
  where
    type_declaration_P' = type_declaration_P structure_P
    and_type_declaration_P' = and_type_declaration_P structure_P
