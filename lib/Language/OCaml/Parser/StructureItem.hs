module Language.OCaml.Parser.StructureItem
  ( structureItemP
  ) where

import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LetBindings
import Language.OCaml.Parser.ModuleBinding
import Language.OCaml.Parser.OpenStatement
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.StrExceptionDeclaration
import Language.OCaml.Parser.TypeDeclarations
import Language.OCaml.Parser.Utils.Types

structureItemP :: Parser Structure -> Parser StructureItem
structureItemP structureP = choice
  [ valOfLetBindings <$> letBindingsP structureP (seqExprP structureP)
  , do
    (nr, l) <- typeDeclarationsP structureP
    return $ mkstrExt (PstrType nr (reverse l)) Nothing -- FIXME: not Nothing
  , do
    (l, _ext) <- strExceptionDeclarationP structureP
    return $ mkstrExt (PstrException l) Nothing -- FIXME
  , do
    (body, _ext) <- openStatementP structureP
    return $ mkstrExt (PstrOpen body) Nothing -- FIXME
  , do
    (body, _ext) <- moduleBindingP structureP
    return $ mkstrExt (PstrModule body) Nothing -- FIXME
  ]
