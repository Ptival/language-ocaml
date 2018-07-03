{

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Generator.Parser
  ( Parser
  , parseExpr
  , parseExprCommaList
  , parseImplementation
  , parseSeqExpr
  ) where

import Data.Default
import Text.Printf

import Language.OCaml.Definitions.Parsing.ASTHelper.Exp as Exp
import Language.OCaml.Definitions.Parsing.ASTHelper.Mod as Mod
import Language.OCaml.Definitions.Parsing.ASTHelper.Opn as Opn
import Language.OCaml.Definitions.Parsing.ASTHelper.Pat as Pat
import Language.OCaml.Definitions.Parsing.ASTHelper.Str as Str
import Language.OCaml.Definitions.Parsing.ASTHelper.Typ as Typ
import Language.OCaml.Definitions.Parsing.ASTHelper.Vb as Vb
import Language.OCaml.Definitions.Parsing.Parser.LetBinding
import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree as ParseTree
import Language.OCaml.Parser.Common
import Language.OCaml.Parser.Generator.Lexer

}

%name unsafeParseExpr           Expr
%name unsafeParseExprCommaList  ExprCommaList
%name unsafeParseImplementation Implementation
%name unsafeParseSeqExpr        SeqExpr

%tokentype { ResultToken }
%lexer { lexWrap } { Located _ TokEOF }
%monad { Alex }
%error { parseError }

%token
  "&&"       { Located _  TokAmperAmper }
  '&'        { Located _  TokAmpersand }
  and        { Located _  TokAnd }
  as         { Located _  TokAs }
  assert     { Located _  TokAssert }
  '`'        { Located _  TokBackQuote }
  '!'        { Located _  TokBang }
  '|'        { Located _  TokBar }
  "||"       { Located _  TokBarBar }
  "|]"       { Located _  TokBarRBracket }
  begin      { Located _  TokBegin }
  CHAR       { Located _ (TokChar $$) }
  class      { Located _  TokClass }
  ':'        { Located _  TokColon }
  "::"       { Located _  TokColonColon }
  ":="       { Located _  TokColonEqual }
  ":>"       { Located _  TokColonGreater }
  ','        { Located _  TokComma }
  constraint { Located _  TokConstraint }
  do         { Located _  TokDo }
  done       { Located _  TokDone }
  '.'        { Located _  TokDot }
  ".."       { Located _  TokDotDot }
  DOTOP      { Located _ (TokDotOp $$) }
  downto     { Located _  TokDownTo }
  else       { Located _  TokElse }
  end        { Located _  TokEnd }
  EOF        { Located _  TokEOF }
  '='        { Located _  TokEqual }
  exception  { Located _  TokException }
  external   { Located _  TokExternal }
  false      { Located _  TokFalse }
  FLOAT      { Located _ (TokFloat $$) }
  for        { Located _  TokFor }
  fun        { Located _  TokFun }
  function   { Located _  TokFunction }
  functor    { Located _  TokFunctor }
  '>'        { Located _  TokGreater }
  ">}"       { Located _  TokGreaterRBrace }
  ">]"       { Located _  TokGreaterRBracket }
  if         { Located _  TokIf }
  in         { Located _  TokIn }
  include    { Located _  TokInclude }
  inherit    { Located _  TokInherit }
  initialier { Located _  TokInitializer }
  INT        { Located _ (TokInt $$) }
  lazy       { Located _  TokLazy }
  '{'        { Located _  TokLBrace }
  "{<"       { Located _  TokLBraceLess }
  '['        { Located _  TokLBracket }
  "[@"       { Located _  TokLBracketAt }
  "[@@"      { Located _  TokLBracketAtAt }
  "[@@@"     { Located _  TokLBracketAtAtAt }
  "[|"       { Located _  TokLBracketBar }
  "[>"       { Located _  TokLBracketGreater }
  "[<"       { Located _  TokLBracketLess }
  "[%"       { Located _  TokLBracketPercent }
  "[%%"      { Located _  TokLBracketPercentPercent }
  '<'        { Located _  TokLess }
  "<-"       { Located _  TokLessMinus }
  let        { Located _  TokLet }
  LIDENT     { Located _ (TokLIdent $$) }
  '('        { Located _  TokLParen }
  match      { Located _  TokMatch }
  method     { Located _  TokMethod }
  '-'        { Located _  TokMinus }
  "-."       { Located _  TokMinusDot }
  "->"       { Located _  TokMinusGreater }
  module     { Located _  TokModule }
  mutable    { Located _  TokMutable }
  new        { Located _  TokNew }
  nonrec     { Located _  TokNonRec }
  object     { Located _  TokObject }
  of         { Located _  TokOf }
  open       { Located _  TokOpen }
  or         { Located _  TokOr }
  '%'        { Located _  TokPercent }
  '+'        { Located _  TokPlus }
  "+."       { Located _  TokPlusDot }
  "+="       { Located _  TokPlusEq }
  private    { Located _  TokPrivate }
  '\''       { Located _  TokQuote }
  '}'        { Located _  TokRBrace }
  ']'        { Located _  TokRBracket }
  rec        { Located _  TokRec }
  ')'        { Located _  TokRParen }
  ';'        { Located _  TokSemi }
  ';;'       { Located _  TokSemiSemi }
  sig        { Located _  TokSig }
  '*'        { Located _  TokStar }
  STRING     { Located _ (TokString $$) }
  struct     { Located _  TokStruct }
  then       { Located _  TokThen }
  '~'        { Located _  TokTilde }
  to         { Located _  TokTo }
  true       { Located _  TokTrue }
  try        { Located _  TokTry }
  type       { Located _  TokType }
  UIDENT     { Located _ (TokUIdent $$) }
  '_'        { Located _  TokUnderscore }
  val        { Located _  TokVal }
  virtual    { Located _  TokVirtual }
  when       { Located _  TokWhen }
  while      { Located _  TokWhile }
  with       { Located _  TokWith }

%nonassoc in
%nonassoc belowSemi
%nonassoc ';'
%nonassoc let
%nonassoc belowWith
%nonassoc function with
%nonassoc and
%nonassoc then
%nonassoc else
%nonassoc "<-"
%right    ":="
%nonassoc as
%left     '|'
%nonassoc belowComma
%left     ','
%right   "->"
%right   or "||"
%right   '&' "&&"
%nonassoc belowEqual
%left     INFIXOP0 '=' '<' '>'
%right    INFIXOP1
%nonassoc belowLBracketAtAt
%nonassoc "[@"
%nonassoc "[@@"
%right    "::"
%left     INFIXOP2 '+' "+." '-' "-." "+="
%left     '%' INFIXOP3 '*'
%right    INFIXOP4
%nonassoc precUnaryMinus precUnaryPlus
%nonassoc precConstantConstructor
%nonassoc precConstrAppl
%nonassoc belowHash
%nonassoc '#'
%left     HASHOP
%nonassoc belowDot
%nonassoc '.' DOTOP
%nonassoc '`' '!' begin CHAR false float int
          '{' "{<" '[' "[|" LIDENT '('
          new PREFIXOP STRING true UIDENT
          "[%" "[%%"

%%

AndLetBinding :: { LetBinding }
  : and Attributes LetBindingBody PostItemAttributes { mklb False $3 ($2 ++ $4) }

Attribute :: { Attribute }
  : "[@@" AttrId Payload ']' { ($2, $3) }

Attributes :: { [Attribute] }
  : {- empty -}          { [] }
  | Attribute Attributes { $1 : $2 }

AttrId :: { Loc String }
  : SingleAttrId { mkLoc $1 (symbolRLoc ()) }
  | SingleAttrId '.' AttrId { mkLoc ($1 ++ "." ++ txt $3) (symbolRLoc ()) }

Constant :: { ParseTree.Constant }
  : INT    { uncurry PconstInteger $1 }
  | CHAR   { PconstChar $1 }
  | STRING { uncurry PconstString  $1 }
  | FLOAT  { uncurry PconstFloat   $1 }

ConstrLongident :: { Longident }
  : ModLongident %prec belowDot   { $1 }
  | ModLongident '.' '(' "::" ')' { Ldot $1 "::" }
  | '[' ']'                       { Lident "[]" }
  | '(' ')'                       { Lident "()" }
  | '(' "::" ')'                  { Lident "::" }
  | false                         { Lident "false" }
  | true                          { Lident "true" }

CoreType :: { CoreType }
  : CoreTypeNoAttr     { $1 }
  -- | CoreType Attribute { attrTyp $1 $2 }

CoreType2 :: { CoreType }
  : SimpleCoreTypeOrTuple { $1 }
  -- TODO

CoreTypeCommaList :: { [CoreType] }
  : CoreType                       { [$1] }
  | CoreTypeCommaList ',' CoreType { $3 : $1 }

CoreTypeList :: { [CoreType] }
  : CoreType                  { [$1] }
  | CoreTypeList '*' CoreType { $3 : $1 }

CoreTypeNoAttr :: { CoreType }
  : CoreType2 %prec "->"    { $1 }
  | CoreType2 as '\'' Ident { mktyp $ PtypAlias $1 $4 }

DirectionFlag :: { DirectionFlag }
  : to     { UpTo }
  | downto { DownTo }

Expr :: { Expression }
  : SimpleExpr %prec belowHash                         { $1 }
  | SimpleExpr SimpleLabeledExprList                   { mkexp $ PexpApply $1 (reverse $2) }
  | LetBindings in SeqExpr                             { exprOfLetBindings $1 $3 }
  -- TODO
  | function ExtAttributes OptBar MatchCases           { mkexpAttrs (PexpFunction (reverse $4)) $2 }
  | fun ExtAttributes LabeledSimplePattern FunDef      { let (l, o, p) = $3 in
                                                         mkexpAttrs (PexpFun l o p $4) $2
                                                       }
  -- | fun ExtAttributes '(' type LidentList ')' FunDef   { mkexpAttrs (pexpDesc $ mkNewtypes $5 $7) $2 }
  | match ExtAttributes SeqExpr with OptBar MatchCases { mkexpAttrs (PexpMatch $3 (reverse $6)) $2 }
  -- TODO: try...
  | ExprCommaList %prec belowComma                     { mkexp $ PexpTuple (reverse $1) }
  | ConstrLongident SimpleExpr %prec belowHash         { mkexp $ PexpConstruct (mkRHS $1 1) (Just $2) }
  -- TODO: name tag
  | if ExtAttributes SeqExpr then Expr else Expr       { mkexpAttrs (PexpIfThenElse $3 $5 (Just $7)) $2 }
  -- | if ExtAttributes SeqExpr then Expr                 { mkexpAttrs (PexpIfThenElse $3 $5 Nothing) $2 }
  | while ExtAttributes SeqExpr do SeqExpr done        { mkexpAttrs (PexpWhile $3 $5) $2 }
  | for ExtAttributes Pattern
    '=' SeqExpr DirectionFlag SeqExpr
    do SeqExpr done                                    { mkexpAttrs (PexpFor $3 $5 $7 $6 $9) $2 }
  -- | Expr "::" Expr                                     { mkexpCons (rhsLoc 2) (ghexp $ PexpTuple [$1, $3]) (symbolRLoc ()) }
  -- TODO: infixops
  | Expr '+'  Expr                                     { mkinfix $1 "+"  $3 }
  | Expr "+." Expr                                     { mkinfix $1 "+." $3 }
  | Expr "+=" Expr                                     { mkinfix $1 "+=" $3 }
  | Expr '-'  Expr                                     { mkinfix $1 "-"  $3 }
  | Expr "-." Expr                                     { mkinfix $1 "-." $3 }
  | Expr '*'  Expr                                     { mkinfix $1 "*"  $3 }
  | Expr '%'  Expr                                     { mkinfix $1 "%"  $3 }
  | Expr '='  Expr                                     { mkinfix $1 "="  $3 }
  | Expr '<'  Expr                                     { mkinfix $1 "<"  $3 }
  | Expr '>'  Expr                                     { mkinfix $1 ">"  $3 }
  | Expr or   Expr                                     { mkinfix $1 "or" $3 }
  | Expr "||" Expr                                     { mkinfix $1 "||" $3 }
  | Expr '&'  Expr                                     { mkinfix $1 "&"  $3 }
  | Expr "&&" Expr                                     { mkinfix $1 "&&" $3 }
  | Expr ":=" Expr                                     { mkinfix $1 ":=" $3 }
  -- TODO
  | SimpleExpr '.' LabelLongident "<-" Expr            { mkexp $ PexpSetField $1 (mkRHS $3 3) $5 }
  -- TODO
  -- | Expr Attribute                                     { attrExp $1 $2 }
  | '_'                                                {% alexError "Wildcard not expected" }

ExtAttributes :: { (Maybe (Loc String), [(Loc String, Payload)]) }
  : {- empty -}           { (Nothing, []) }
  | Attribute Attributes  { (Nothing, $1 : $2) }
  | '%' AttrId Attributes { (Just $2, $3) }

ExprCommaList :: { [Expression] }
  : Expr ',' Expr          { [$3, $1] }
  | ExprCommaList ',' Expr { $3 : $1 }

FunBinding :: { Expression }
  : StrictBinding { $1 }
  -- | TypeConstraint '=' SeqExpr { mkexpConstraint $3 $1 }

FunDef :: { Expression }
  : "->" SeqExpr                    { $2 }
  | ':' SimpleCoreType "->" SeqExpr { Exp.mk def $ PexpConstraint $4 $2 }
  | LabeledSimplePattern FunDef { let (l, o, p) = $1 in
                                  ghexp $ PexpFun l o p $2
                                }

Ident :: { String }
  : UIDENT { $1 }
  | LIDENT { $1 }

Implementation :: { () }
  : Structure EOF { error "TODO" }

LabelLongident :: { Longident }
  : LIDENT                  { Lident $1 }
  | ModLongident '.' LIDENT { Ldot $1 $3 }

LabeledSimpleExpr :: { (ArgLabel, Expression) }
  : SimpleExpr %prec belowHash { (Nolabel, $1) }
  -- | LabelExpr                  { $1 }

LabeledSimplePattern :: { (ArgLabel, Maybe Expression, Pattern) }
  -- TODO
  : SimplePattern { (Nolabel, Nothing, $1) }

LetBinding :: { LetBindings }
  : let ExtAttributes RecFlag LetBindingBody PostItemAttributes { let (ext, attr) = $2 in
                                                                  mklbs ext $3 (mklb True $4 (attr ++ $5))
                                                                }

LetBindingBody :: { (Pattern, Expression) }
  : ValIdent StrictBinding { (mkpatvar $1 1, $2) }
  -- TODO

LetBindings :: { LetBindings }
  : LetBinding                { $1 }
  -- | LetBindings AndLetBinding { addlb $1 $2 }

MatchCase :: { Case }
  : Pattern "->" SeqExpr { Exp.case' def $1 $3 }
  -- | Pattern when SeqExpr "->" SeqExpr { Exp.case' (def { guard = $3 }) $1 $5 }
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

OpenStatement :: { (OpenDescription, Maybe (Loc String)) }
  : open OverrideFlag ExtAttributes ModLongident PostItemAttributes
  { let (ext, attrs) = $3 in
    (Opn.mk (def { override = $2
                 , attrs    = attrs ++ $5
                 , loc      = symbolRLoc ()
                 , docs     = symbolDocs ()
                 })
          (mkRHS $4 4)
    , ext
    )
  }


OptBar :: { () }
  : {- empty -} { () }
  | '|'         { () }

OverrideFlag :: { OverrideFlag }
  : {- empty -} { Fresh }
  | '!'         { Override }

Pattern :: { Pattern }
  : Pattern as ValIdent                                  { mkpat $ PpatAlias $1 (mkRHS $3 3) }
  | Pattern as                                           {% alexError "Pattern as <ERROR>" }
  | PatternCommaList %prec belowComma                    { mkpat $ PpatTuple (reverse $1) }
  | Pattern "::" Pattern                                 { mkpatCons (rhsLoc 2) (ghpat $ PpatTuple [$1, $3]) (symbolRLoc ()) }
  | Pattern "::"                                         {% alexError "Pattern :: <ERROR>" }
  | Pattern '|' Pattern                                  { mkpat $ PpatOr $1 $3 }
  | Pattern '|'                                          {% alexError "Pattern | <ERROR>" }
  -- | exception ExtAttributes Pattern %prec precConstrAppl { mkpatAttrs (PpatException $3) $2 }
  -- | Pattern Attribute                                    { attrPat $1 $2 }
  | PatternGen                                           { $1 }

PatternCommaList :: { [Pattern] }
  : PatternCommaList ',' Pattern { $3 : $1 }
  | Pattern ',' Pattern          { [$3, $1] }
  | Pattern ','                  {% alexError "Pattern , <ERROR>" }

PatternGen :: { Pattern }
  : SimplePattern                                { $1 }
  | ConstrLongident Pattern %prec precConstrAppl { mkpat $ PpatConstruct (mkRHS $1 1) (Just $2) }
  -- | NameTag Pattern %prec precConstrAppl         { mkpat $ PpatVariant $1 (Just $2) }
  -- | lazy ExtAttributes SimplePattern             { mkpatAttrs (PpatLazy $3) $2 }

Payload :: { Payload }
  : Structure { PStr $1 }
  -- TODO

PostItemAttribute :: { (Loc String, Payload) }
  : "[@@" AttrId Payload ']' { ($2, $3) }

PostItemAttributes :: { Attributes }
  : {- empty -}                          { [] }
  | PostItemAttribute PostItemAttributes { $1 : $2 }

RecFlag :: { RecFlag }
  : {- empty -} { NonRecursive }
  | rec         { Recursive }

SeqExpr :: { Expression }
  : Expr %prec belowSemi        { $1 }
  | Expr ';'                    { $1 }
  | Expr ';' SeqExpr            { mkexp $ PexpSequence $1 $3 }
  | Expr ';' '%' AttrId SeqExpr { let seq = mkexp $ PexpSequence $1 $5 in
                                  let payload = PStr [mkstrexp seq []] in
                                  mkexp $ PexpExtension ($4, payload)
                                }

SimpleCoreType :: { CoreType }
  : SimpleCoreType2 %prec belowHash           { $1 }
  | '(' CoreTypeCommaList ')' %prec belowHash {% case $2 of
                                                 [sty] -> return sty
                                                 _     -> alexError "( CoreTypeCommaList ) [expected length 1]"
                                              }

SimpleCoreType2 :: { CoreType }
  : '\'' Ident                              { mktyp $ PtypVar $2 }
  | '_'                                     { mktyp $ PtypAny }
  | TypeLongident                           { mktyp $ PtypConstr (mkRHS $1 1) [] }
  | SimpleCoreType2 TypeLongident           { mktyp $ PtypConstr (mkRHS $2 2) [$1] }
  | '(' CoreTypeCommaList ')' TypeLongident { mktyp $ PtypConstr (mkRHS $4 4) (reverse $2) }
  -- TODO
  | "[>" ']'                                { mktyp $ PtypVariant [] Open Nothing }

SimpleCoreTypeOrTuple :: { CoreType }
  : SimpleCoreType                  { $1 }
  | SimpleCoreType '*' CoreTypeList { mktyp $ PtypTuple ($1 : reverse $3) }

SimpleExpr :: { Expression }
  : ValLongident                                  { mkexp $ PexpIdent (mkRHS $1 1) }
  | Constant                                      { mkexp $ PexpConstant $1 }
  | ConstrLongident %prec precConstantConstructor { mkexp $ PexpConstruct (mkRHS $1 1) Nothing }
  -- TODO
  | '(' SeqExpr ')'                               { relocExp $2 }
  | '(' SeqExpr                                   {% alexError "( SeqExpr <ERROR>" }
  -- | begin ExtAttributes SeqExpr end               { wrapExtAttrs (relocExp $3) $2 }
  -- | begin ExtAttributes end                       { Exp.mkAttrs (PexpConstruct (mkLoc (Lident "()") (symbolRLoc ())) Nothing) $2 }
  -- | begin ExtAttributes                           {% alexError "begin ExtAttributes <ERROR>" }
  -- | '(' SeqExpr TypeConstraint ')'                { Exp.mkConstraint $2 $3 }
  | SimpleExpr '.' LabelLongident                 { mkexp $ PexpField $1 (mkRHS $3 3) }
  -- | ModLongident '.' '(' SeqExpr ')'              { mkexp $ PexpOpen Fresh (mkRHS $1 1) $4 }
  -- TODO

SimpleLabeledExprList :: { [(ArgLabel, Expression)] }
  : LabeledSimpleExpr                       { [$1] }
  | SimpleLabeledExprList LabeledSimpleExpr { $2 : $1 }

SimplePattern :: { Pattern }
  : ValIdent %prec belowEqual { mkpat $ PpatVar (mkRHS $1 1) }
  | SimplePatternNotIdent     { $1 }

SimplePatternNotIdent :: { Pattern }
  : '_' { mkpat $ PpatAny }
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

StrictBinding :: { Expression }
  : '=' SeqExpr { $2 }
  | LabeledSimplePattern FunBinding { let (l, o, p) = $1 in
                                      ghexp $ PexpFun l o p $2
                                    }
  -- | '(' LidentList ')' FunBinding   { mk_newtypes $3 $5 }

Structure :: { Structure }
  : SeqExpr PostItemAttributes StructureTail { do
                                               -- TODO: mark_rhsDocs 1 2
                                               (textStr 1) ++ mkstrexp $1 $2 : $3
                                             }
  | StructureTail                            { $1 }

StructureItem :: { StructureItem }
  : LetBindings { valOfLetBindings $1 }
  -- TODO

StructureTail :: { Structure }
  : {- empty -}                 { [] }
  | "::" Structure              { (textStr 1) ++ $2 }
  | StructureItem StructureTail { (textStr 1) ++ $1 : $2 }

TypeConstraint :: { (Maybe CoreType, Maybe CoreType) }
  : ':' CoreType               { (Just $2, Nothing) }
  | ':' CoreType ":>" CoreType { (Just $2, Just $4) }
  | ":>" CoreType              { (Nothing, Just $2) }
  | ':'                        {% alexError ": <ERROR>" }
  | ":>"                       {% alexError ":> <ERROR>" }

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

parseError :: ResultToken -> Alex a
parseError (Located (SrcSpan {..}) tok) = do
  alexError $ printf "unexpected token %s at line %s, column %s"
    (show tok) (show srcSpanStartLine) (show srcSpanStartCol)

type Parser a = String -> Either String a

parseExpr :: Parser Expression
parseExpr = safeParse unsafeParseExpr

parseExprCommaList :: Parser [Expression]
parseExprCommaList = safeParse unsafeParseExprCommaList

parseImplementation :: Parser ()
parseImplementation = safeParse unsafeParseImplementation

parseSeqExpr :: Parser Expression
parseSeqExpr = safeParse unsafeParseSeqExpr

safeParse :: Alex a -> String -> Either String a
safeParse = flip runAlex

}
