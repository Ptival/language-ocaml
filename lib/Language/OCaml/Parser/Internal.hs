module Language.OCaml.Parser.Internal
  ( Parser
  , constant_P
  , constr_ident_P
  , constr_longident_P
  , expr_P
  , ident_P
  , implementation_P
  , labeled_simple_expr_P
  , let_binding_P
  , match_case_P
  , mod_longident_P
  , open_statement_P
  , pattern_P
  , seq_expr_P
  , simple_expr_P
  , simple_labeled_expr_list_P
  , simple_pattern_P
  , string_T
  , structure_P
  , structure_item_P
  , type_declaration_P
  , val_ident_P
  , val_longident_P
  ) where

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.Constant
import           Language.OCaml.Parser.ConstrIdent
import           Language.OCaml.Parser.ConstrLongident
import qualified Language.OCaml.Parser.Expr
import           Language.OCaml.Parser.Implementation
import qualified Language.OCaml.Parser.LabeledSimpleExpr
import           Language.OCaml.Parser.LetBinding
import qualified Language.OCaml.Parser.MatchCase
import           Language.OCaml.Parser.ModLongident
import           Language.OCaml.Parser.OpenStatement
import           Language.OCaml.Parser.Pattern
import qualified Language.OCaml.Parser.SeqExpr
import qualified Language.OCaml.Parser.SimpleExpr
import qualified Language.OCaml.Parser.SimpleLabeledExprList
import qualified Language.OCaml.Parser.SimplePattern
import           Language.OCaml.Parser.Structure
import qualified Language.OCaml.Parser.StructureItem
import           Language.OCaml.Parser.Tokens
import qualified Language.OCaml.Parser.TypeDeclaration
import           Language.OCaml.Parser.ValIdent
import           Language.OCaml.Parser.ValLongident
import           Language.OCaml.Parser.Utils.Types

-- Tying the knots for our clients!

expr_P :: Parser Expression
expr_P = Language.OCaml.Parser.Expr.expr_P structure_P seq_expr_P

labeled_simple_expr_P :: Parser (ASTTypes.Arg_label, Expression)
labeled_simple_expr_P =
  Language.OCaml.Parser.LabeledSimpleExpr.labeled_simple_expr_P seq_expr_P

match_case_P :: Parser Case
match_case_P = Language.OCaml.Parser.MatchCase.match_case_P seq_expr_P

seq_expr_P :: Parser Expression
seq_expr_P = Language.OCaml.Parser.SeqExpr.seq_expr_P structure_P

simple_expr_P :: Parser Expression
simple_expr_P = Language.OCaml.Parser.SimpleExpr.simple_expr_P seq_expr_P

simple_labeled_expr_list_P :: Parser [(ASTTypes.Arg_label, Expression)]
simple_labeled_expr_list_P =
  Language.OCaml.Parser.SimpleLabeledExprList.simple_labeled_expr_list_P seq_expr_P

simple_pattern_P :: Parser Pattern
simple_pattern_P = Language.OCaml.Parser.SimplePattern.simple_pattern_P pattern_P

structure_item_P :: Parser Structure_item
structure_item_P = Language.OCaml.Parser.StructureItem.structure_item_P structure_P

type_declaration_P :: Parser (ASTTypes.Rec_flag, Type_declaration)
type_declaration_P = Language.OCaml.Parser.TypeDeclaration.type_declaration_P structure_P
