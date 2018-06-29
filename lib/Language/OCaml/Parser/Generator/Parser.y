{

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Generator.Parser
  ( Parser
  , parseImplementation
  ) where

import Data.Default
import Text.Printf

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Generator.Lexer

}

%name unsafeParseImplementation Implementation

%tokentype { Located Token }
%lexer { lexWrap } { Located _ TokEOF }
%monad { Alex }
%error { parseError }

%token
  and        { Located _  TokAnd }
  as         { Located _  TokAs }
  '|'        { Located _  TokBar }
  ':'        { Located _  TokColon }
  "::"       { Located _  TokColonColon }
  ','        { Located _  TokComma }
  '.'        { Located _  TokDot }
  EOF        { Located _  TokEOF }
  '='        { Located _  TokEqual }
  fun        { Located _  TokFun }
  function   { Located _  TokFunction }
  "->"       { Located _  TokMinusGreater }
  let        { Located _  TokLet }
  "[@@"      { Located _  TokLBracketAtAt }
  '('        { Located _  TokLParen }
  LIDENT     { Located _ (TokLIdent $$) }
  in         { Located _  TokIn }
  initialier { Located _  TokInitializer }
  '%'        { Located _  TokPercent }
  '\''       { Located _  TokQuote }
  ']'        { Located _  TokRBracket }
  rec        { Located _  TokRec }
  ')'        { Located _  TokRParen }
  ';'        { Located _  TokSemi }
  '*'        { Located _  TokStar }
  UIDENT     { Located _ (TokUIdent $$) }
  '_'        { Located _  TokUnderscore }

-- %nonassoc in
%nonassoc below_SEMI
%nonassoc ';'
%nonassoc let
%nonassoc below_WITH
%nonassoc function with
%nonassoc and
%nonassoc then
%nonassoc else
%nonassoc "<-"
%right    ":="
%nonassoc as
%left     '|'
%nonassoc below_COMMA
%left     ','
%right   "->"
%right   or "||"
%right   '&' "&&"
%nonassoc below_EQUAL
%left     INFIXOP0 '=' '<' '>'
%right    INFIXOP1
%nonassoc below_LBRACKETATAT
%nonassoc "[@"
%nonassoc "[@@"
%right    "::"
%left     INFIXOP2 '+' "+." '-' "-." "+="
%left     '%' INFIXOP3 '*'
%right    INFIXOP4
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc prec_constant_constructor
%nonassoc prec_constr_appl
%nonassoc below_HASH
%nonassoc '#'
%left     HASHOP
%nonassoc below_DOT
%nonassoc '.' DOTOP
%nonassoc '`' '!' begin CHAR false float int
          '{' "{<" '[' "[|" LIDENT '('
          new PREFIXOP STRING true UIDENT
          "[%" "[%%"

%%

AndLetBinding :: { a }
  : and Attributes LetBindingBody PostItemAttributes { mklb False $3 ($2 ++ $4) }

Attribute :: { b }
  : "[@@" AttrId Payload ']' { ($2, $3) }

Attributes :: { [c] }
  : {- empty -}          { [] }
  | Attribute Attributes { $1 : $2 }

AttrId :: { d }
  : SingleAttrId { mkLoc $1 (symbol_rloc ()) }
  | SingleAttrId '.' AttrId { mkLoc ($1 ++ "." ++ txt $3) (symbol_rloc ()) }

CoreType :: { Core_type }
  : CoreTypeNoAttr     { $1 }
  -- | CoreType Attribute { attrTyp $1 $2 }

CoreType2 :: { Core_type }
  : SimpleCoreTypeOrTuple { $1 }
  -- TODO

CoreTypeCommaList :: { [Core_type] }
  : CoreType                       { [$1] }
  | CoreTypeCommaList ',' CoreType { $3 : $1 }

CoreTypeList :: { [Core_type] }
  : CoreType                  { [$1] }
  | CoreTypeList '*' CoreType { $3 : $1 }

CoreTypeNoAttr :: { Core_type }
  : CoreType2               %prec "->" { $1 }
  | CoreType2 as '\'' Ident            { mktyp $ Ptyp_alias $1 $4 }

Expr :: { e }
  : SimpleExpr                               %prec below_HASH { $1 }
  | SimpleExpr SimpleLabeledExprList                          { mkexp $ Pexp_apply $1 (reverse $2) }
  | LetBindings in SeqExpr                                    { expr_of_let_bindings $1 $3 }
  -- TODO
  | function ExtAttributes OptBar MatchCases                  { mkexp_attrs (Pexp_function (reverse $4)) $2 }
  | fun ExtAttributes LabeledSimplePattern FunDef             { let (l, o, p) = $3 in
                                                                mkexp_attrs (Pexp_fun l o p $4) $2
                                                              }

ExtAttributes :: { f }
  : {- empty -}           { (Nothing, []) }
  | Attribute Attributes  { (Nothing, $1 : $2) }
  | '%' AttrId Attributes { (Just $2, $3) }

FunBinding :: { Expression }
  : StrictBinding { $1 }
  -- | TypeConstraint '=' SeqExpr { mkexp_constraint $3 $1 }

FunDef :: { Expression }
  : "->" SeqExpr                    { $2 }
  | ':' SimpleCoreType "->" SeqExpr { mkExp def $ Pexp_constraint $4 $2 }
  | LabeledSimplePattern FunDef { let (l, o, p) = $1 in
                                  ghexp $ Pexp_fun l o p $2
                                }

Ident :: { String }
  : UIDENT { $1 }
  | LIDENT { $1 }

Implementation :: { g }
  : Structure EOF { error "TODO" }

LabeledSimpleExpr :: { h }
  : SimpleExpr %prec below_HASH { (Nolabel, $1) }
  -- | LabelExpr                   { $1 }

LabeledSimplePattern :: { (Arg_label, Maybe Expression, Pattern) }
  -- TODO
  : SimplePattern { (Nolabel, Nothing, $1) }

LetBinding :: { i }
  : let ExtAttributes RecFlag LetBindingBody PostItemAttributes { let (ext, attr) = $2 in
                                                                  mklbs ext $3 (mklb True $4 (attr ++ $5))
                                                                }

LetBindingBody :: { j }
  : ValIdent StrictBinding { (mkpatvar $1 1, $2) }
  -- TODO

LetBindings :: { k }
  : LetBinding                { $1 }
  -- | LetBindings AndLetBinding { addlb $1 $2 }

MatchCase :: { Case }
  : Pattern "->" SeqExpr { caseExp def $1 $3 }
  -- | Pattern when SeqExpr "->" SeqExpr { caseExp (def { guard = $3 }) $1 $5 }
  -- TODO

MatchCases :: { [Case] }
  : MatchCase                { [$1] }
  | MatchCases '|' MatchCase { $3 : $1 }

ModExtLongident :: { Longident }
  : UIDENT                                  { Lident $1 }
  | ModExtLongident '.' UIDENT              { Ldot $1 $3 }
  -- | ModExtLongident '(' ModExtLongident ')' { lapply $1 $3 }

ModLongident :: { Longident }
  : UIDENT                  { Lident $1 }
  | ModLongident '.' UIDENT { Ldot $1 $3 }

OptBar :: { () }
  : {- empty -} { () }
  | '|'         { () }

Pattern :: { Pattern }
  : Pattern as ValIdent                    { mkpat $ Ppat_alias $1 (mkRHS $3 3) }
  | Pattern as                             {% alexError "Pattern as <!> ValIdent" }
  | PatternCommaList     %prec below_COMMA { mkpat $ Ppat_tuple (reverse $1) }
  | Pattern "::" Pattern                   { mkpat_cons (rhsLoc 2) (ghpat $ Ppat_tuple [$1, $3]) (symbol_rloc ()) }

PatternCommaList :: { [Pattern] }
  : PatternCommaList ',' Pattern { $3 : $1 }
  | Pattern ',' Pattern          { [$3, $1] }
  | Pattern ','                  {% alexError "Pattern , <!> Pattern" }

Payload :: { l }
  : Structure { PStr $1 }
  -- TODO

PostItemAttribute :: { m }
  : "[@@" AttrId Payload ']' { ($2, $3) }

PostItemAttributes :: { Attributes }
  : {- empty -}                          { [] }
  | PostItemAttribute PostItemAttributes { $1 : $2 }

RecFlag :: { n }
  : {- empty -} { NonRecursive }
  | rec         { Recursive }

SeqExpr :: { o }
  : Expr %prec below_SEMI { $1 }
  | Expr ';'              { $1 }
  | Expr ';' SeqExpr      { mkexp $ Pexp_sequence $1 $3 }

SimpleCoreType :: { Core_type }
  : SimpleCoreType2           %prec below_HASH { $1 }
  | '(' CoreTypeCommaList ')' %prec below_HASH
    {% case $2 of
       [sty] -> return sty
       _     -> alexError "( CoreTypeCommaList ) [expected length 1]"
    }

SimpleCoreType2 :: { Core_type }
  : '\'' Ident                              { mktyp $ Ptyp_var $2 }
  | '_'                                     { mktyp $ Ptyp_any }
  | TypeLongident                           { mktyp $ Ptyp_constr (mkRHS $1 1) [] }
  | SimpleCoreType2 TypeLongident           { mktyp $ Ptyp_constr (mkRHS $2 2) [$1] }
  -- | '(' CoreTypeCommaList ')' TypeLongident { mktyp $ Ptyp_constr (mkRHS $4 4) (reverse $2) }
  -- TODO

SimpleCoreTypeOrTuple :: { Core_type }
  : SimpleCoreType                  { $1 }
  | SimpleCoreType '*' CoreTypeList { mktyp $ Ptyp_tuple ($1 : reverse $3) }

SimpleExpr :: { p }
  : ValLongident { mkexp $ Pexp_ident (mkRHS $1 1) }
  -- TODO

SimpleLabeledExprList :: { [(Arg_label, Expression)] }
  : LabeledSimpleExpr                       { [$1] }
  | SimpleLabeledExprList LabeledSimpleExpr { $2 : $1 }

SimplePattern :: { Pattern }
  : ValIdent %prec below_EQUAL { mkpat $ Ppat_var (mkRHS $1 1) }
  | SimplePatternNotIdent      { $1 }

SimplePatternNotIdent :: { Pattern }
  : '_' { mkpat $ Ppat_any }
  -- TODO

SingleAttrId :: { String }
  : LIDENT { $1 }
  | UIDENT { $1 }
  -- | and    { "and" }
  -- | as     { "as" }
  -- | assert { "assert" }
  -- | begin  { "begin" }
  -- | class  { "class" }
  -- TODO

StrictBinding :: { q }
  : '=' SeqExpr { $2 }
  | LabeledSimplePattern FunBinding { do
                                      let (l, o, p) = $1
                                      ghexp $ Pexp_fun l o p $2
                                    }
  -- TODO

Structure :: { Structure }
  : SeqExpr PostItemAttributes StructureTail { do
                                               -- TODO: mark_rhs_docs 1 2
                                               (text_str 1) ++ mkstrexp $1 $2 : $3
                                             }
  | StructureTail                            { $1 }

StructureItem :: { Structure_item }
  : LetBindings { val_of_let_bindings $1 }
  -- TODO

StructureTail :: { Structure }
  : {- empty -}                 { [] }
  | "::" Structure              { (text_str 1) ++ $2 }
  | StructureItem StructureTail { (text_str 1) ++ $1 : $2 }

TypeLongident :: { Longident }
  : LIDENT                     { Lident $1 }
  | ModExtLongident '.' LIDENT { Ldot $1 $3 }

ValIdent :: { r }
  : LIDENT { $1 }
  -- | '(' Operator ')' { TODO }
  -- TODO

ValLongident :: { s }
  : ValIdent                  { Lident $1 }
  | ModLongident '.' ValIdent { Ldot $1 $3 }

{

parseError :: Located Token -> Alex a
parseError (Located (SrcSpan {..}) tok) = do
  alexError $ printf "unexpected token %s at line %s, column %s"
    (show tok) (show srcSpanStartLine) (show srcSpanStartCol)

type Parser a = String -> Either String a

parseImplementation :: Parser ()
parseImplementation = safeParse unsafeParseImplementation

safeParse :: Alex a -> String -> Either String a
safeParse = flip runAlex

}
