module Language.OCaml.Parser.Internal
  ( module Language.OCaml.Parser.Tokens
  , Parser
  , constant_P
  , constr_ident_P
  , constr_longident_P
  , constructor_arguments_P
  , constructor_declarations_P
  , core_type_P
  , expr_P
  , generalized_constructor_arguments_P
  , ident_P
  , implementation_P
  , labeled_simple_expr_P
  , let_binding_P
  , let_binding_body_P
  , match_case_P
  , mod_longident_P
  , open_statement_P
  , pattern_P
  , pattern_no_exn_P
  , post_item_attributes_P
  , seq_expr_P
  , simple_expr_P
  , simple_labeled_expr_list_P
  , simple_pattern_P
  , structure_P
  , structure_item_P
  , type_declaration_P
  , type_declarations_P
  , type_kind_P
  , val_ident_P
  , val_longident_P
  ) where

import           Language.OCaml.Definitions.Parsing.ASTTypes
import           Language.OCaml.Definitions.Parsing.Parser.LetBindings
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.Constant
import           Language.OCaml.Parser.ConstrIdent
import           Language.OCaml.Parser.ConstrLongident
import qualified Language.OCaml.Parser.ConstructorArguments
import qualified Language.OCaml.Parser.ConstructorDeclarations
import           Language.OCaml.Parser.CoreType
import qualified Language.OCaml.Parser.Expr
import qualified Language.OCaml.Parser.GeneralizedConstructorArguments
import           Language.OCaml.Parser.Implementation
import qualified Language.OCaml.Parser.LabeledSimpleExpr
import qualified Language.OCaml.Parser.LetBinding
import qualified Language.OCaml.Parser.LetBindingBody
import qualified Language.OCaml.Parser.MatchCase
import           Language.OCaml.Parser.ModLongident
import           Language.OCaml.Parser.OpenStatement
import           Language.OCaml.Parser.Pattern
import           Language.OCaml.Parser.PatternNoExn
import qualified Language.OCaml.Parser.PostItemAttributes
import qualified Language.OCaml.Parser.SeqExpr
import qualified Language.OCaml.Parser.SimpleExpr
import qualified Language.OCaml.Parser.SimpleLabeledExprList
import qualified Language.OCaml.Parser.SimplePattern
import           Language.OCaml.Parser.Structure
import qualified Language.OCaml.Parser.StructureItem
import           Language.OCaml.Parser.Tokens
import qualified Language.OCaml.Parser.TypeDeclaration
import qualified Language.OCaml.Parser.TypeDeclarations
import qualified Language.OCaml.Parser.TypeKind
import           Language.OCaml.Parser.ValIdent
import           Language.OCaml.Parser.ValLongident
import           Language.OCaml.Parser.Utils.Types

-- Tying the knots for our clients!

constructor_arguments_P :: Parser Constructor_arguments
constructor_arguments_P =
  Language.OCaml.Parser.ConstructorArguments.constructor_arguments_P core_type_P

constructor_declarations_P :: Parser [Constructor_declaration]
constructor_declarations_P =
  Language.OCaml.Parser.ConstructorDeclarations.constructor_declarations_P structure_P core_type_P

expr_P :: Parser Expression
expr_P = Language.OCaml.Parser.Expr.expr_P structure_P seq_expr_P

generalized_constructor_arguments_P :: Parser (Constructor_arguments, Maybe a)
generalized_constructor_arguments_P =
  Language.OCaml.Parser.GeneralizedConstructorArguments.generalized_constructor_arguments_P
  core_type_P

labeled_simple_expr_P :: Parser (Arg_label, Expression)
labeled_simple_expr_P =
  Language.OCaml.Parser.LabeledSimpleExpr.labeled_simple_expr_P seq_expr_P

let_binding_P :: Parser Let_bindings
let_binding_P =
  Language.OCaml.Parser.LetBinding.let_binding_P structure_P seq_expr_P

let_binding_body_P :: Parser (Pattern, Expression)
let_binding_body_P =
  Language.OCaml.Parser.LetBindingBody.let_binding_body_P seq_expr_P

match_case_P :: Parser Case
match_case_P = Language.OCaml.Parser.MatchCase.match_case_P seq_expr_P

post_item_attributes_P :: Parser [(Loc String, Payload)]
post_item_attributes_P = Language.OCaml.Parser.PostItemAttributes.post_item_attributes_P structure_P

seq_expr_P :: Parser Expression
seq_expr_P = Language.OCaml.Parser.SeqExpr.seq_expr_P structure_P

simple_expr_P :: Parser Expression
simple_expr_P = Language.OCaml.Parser.SimpleExpr.simple_expr_P seq_expr_P

simple_labeled_expr_list_P :: Parser [(Arg_label, Expression)]
simple_labeled_expr_list_P =
  Language.OCaml.Parser.SimpleLabeledExprList.simple_labeled_expr_list_P seq_expr_P

simple_pattern_P :: Parser Pattern
simple_pattern_P = Language.OCaml.Parser.SimplePattern.simple_pattern_P pattern_P

structure_item_P :: Parser Structure_item
structure_item_P = Language.OCaml.Parser.StructureItem.structure_item_P structure_P

type_declaration_P :: Parser (Rec_flag, Type_declaration)
type_declaration_P = Language.OCaml.Parser.TypeDeclaration.type_declaration_P structure_P

type_declarations_P :: Parser (Rec_flag, [Type_declaration])
type_declarations_P = Language.OCaml.Parser.TypeDeclarations.type_declarations_P structure_P

type_kind_P :: Parser (Type_kind, Private_flag, Maybe Core_type)
type_kind_P = Language.OCaml.Parser.TypeKind.type_kind_P structure_P
