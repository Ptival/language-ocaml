module Language.OCaml.Parser.StructureItem
  ( structure_item_P
  ) where

import Data.Text.Prettyprint.Doc
import Text.Megaparsec
import Text.Megaparsec.String

import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.LetBindings
import Language.OCaml.Parser.ModuleBinding
import Language.OCaml.Parser.OpenStatement
import Language.OCaml.Parser.SeqExpr
import Language.OCaml.Parser.TypeDeclarations
import Language.OCaml.PrettyPrinter ()

structure_item_P :: Parser Structure -> Parser Structure_item
structure_item_P structure_P = choice
  [ val_of_let_bindings <$> let_bindings_P structure_P (seq_expr_P structure_P)
  , do
    (nr, l) <- type_declarations_P structure_P
    return $ mkstr_ext (Pstr_type nr (checkReverse l)) Nothing -- FIXME: not Nothing
  , do
    (body, ext) <- open_statement_P structure_P
    return $ mkstr_ext (Pstr_open body) Nothing -- FIXME
  , do
    (body, ext) <- module_binding_P structure_P
    return $ mkstr_ext (Pstr_module body) Nothing -- FIXME
  ]
  where
    checkReverse [] = []
    checkReverse [x] = [x]
    checkReverse l =
      error $ "reverse?\n" ++ (show $ pretty l)
