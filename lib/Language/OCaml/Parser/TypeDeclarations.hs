module Language.OCaml.Parser.TypeDeclarations
  ( typeDeclarationsP
  ) where

import qualified Language.OCaml.Definitions.Parsing.ASTTypes  as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.AndTypeDeclaration
import           Language.OCaml.Parser.TypeDeclaration
import           Language.OCaml.Parser.Utils.Combinators
import           Language.OCaml.Parser.Utils.Types

typeDeclarationsP ::
  Parser Structure -> Parser (ASTTypes.RecFlag, [TypeDeclaration])
typeDeclarationsP structureP = leftRecursive
  [ do
    (nonrecFlag, ty) <- typeDeclarationP'
    return $ (nonrecFlag, [ty])
  ]
  [ do
    ty <- andTypeDeclarationP'
    return $ \ (nonrecFlag, tys) -> (nonrecFlag, ty : tys)
  ]
  where
    typeDeclarationP' = typeDeclarationP structureP
    andTypeDeclarationP' = andTypeDeclarationP structureP
