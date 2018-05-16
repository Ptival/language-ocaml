module Language.OCaml.Parser.Internal
  ( constr_ident_P
  , constr_longident_P
  , expr_P
  , ident_P
  , implementation_P
  , let_binding_P
  , match_case_P
  , mod_longident_P
  , open_statement_P
  , pattern_P
  , seq_expr_P
  , simple_expr_P
  , simple_pattern_P
  , string_T
  , structure_P
  , structure_item_P
  , val_ident_P
  , val_longident_P
  ) where

import           Text.Megaparsec.String

import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Parser.Common
import           Language.OCaml.Parser.ConstrIdent
import           Language.OCaml.Parser.ConstrLongident
import qualified Language.OCaml.Parser.Expr
import           Language.OCaml.Parser.Implementation
import           Language.OCaml.Parser.LetBinding
import           Language.OCaml.Parser.MatchCase
import           Language.OCaml.Parser.ModLongident
import           Language.OCaml.Parser.OpenStatement
import           Language.OCaml.Parser.Pattern
import qualified Language.OCaml.Parser.SeqExpr
import qualified Language.OCaml.Parser.SimpleExpr
import           Language.OCaml.Parser.SimplePattern
import           Language.OCaml.Parser.Structure
import           Language.OCaml.Parser.StructureItem
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.ValIdent
import           Language.OCaml.Parser.ValLongident

-- Tying the knots for our clients!

expr_P :: Parser Expression
expr_P = Language.OCaml.Parser.Expr.expr_P structure_P seq_expr_P

seq_expr_P :: Parser Expression
seq_expr_P = Language.OCaml.Parser.SeqExpr.seq_expr_P structure_P

simple_expr_P :: Parser Expression
simple_expr_P = Language.OCaml.Parser.SimpleExpr.simple_expr_P seq_expr_P
