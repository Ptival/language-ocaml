{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Exp
  ( CaseOpts(..)
  , MkOpts(..)
  , apply
  , array
  , assert
  , attr
  , case'
  , coerce
  , constant
  , constraint
  , construct
  , extension
  , field
  , for
  , fun
  , function
  , ident
  , ifThenElse
  , lazy
  , let'
  , letException
  , letModule
  , match
  , mk
  , new
  , newType
  , object
  , open
  , override
  , pack
  , poly
  , record
  , send
  , sequence
  , setField
  , setInstVar
  , try
  , tuple
  , unreachable
  , variant
  , while
  ) where

import Data.Default
import Prelude                                             hiding (sequence)

import Language.OCaml.Definitions.Parsing.ASTHelper.Common
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree        as ParseTree
import Language.OCaml.Definitions.Parsing.Location

attr :: Expression -> Attribute -> Expression
attr d a = d { pexpAttributes = pexpAttributes d ++ [a] }

-- oops! case is a reserved Haskell lexeme
case' :: CaseOpts -> Pattern -> Expression -> Case
case' (CaseOpts {..}) lhs rhs = Case
  { pcLHS   = lhs
  , pcGuard = guard
  , pcRHS   = rhs
  }

data CaseOpts = CaseOpts
  { guard :: Maybe Expression
  }

instance Default CaseOpts where
  def = CaseOpts
    { guard  = Nothing
    }

mk :: MkOpts -> ExpressionDesc -> Expression
mk (MkOpts {..}) desc =
  Expression
  { pexpDesc       = desc
  , pexpLoc        = loc
  , pexpAttributes = attrs
  }

data MkOpts = MkOpts
  { attrs  :: [Attribute]
  , loc    :: Location
  }

instance Default MkOpts where
  def = MkOpts
    { attrs  = []
    , loc    = defaultLoc
    }

ident :: MkOpts -> Loc Longident -> Expression
ident (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpIdent a

constant :: MkOpts -> ParseTree.Constant -> Expression
constant (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpConstant a

let' :: MkOpts -> RecFlag -> [ValueBinding] -> Expression -> Expression
let' (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpLet a b c

fun :: MkOpts -> ArgLabel -> Maybe Expression -> Pattern -> Expression -> Expression
fun (MkOpts {..}) a b c d = mk (def { loc, attrs }) $ PexpFun a b c d

function :: MkOpts -> [Case] -> Expression
function (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpFunction a

apply :: MkOpts -> Expression -> [(ArgLabel, Expression)] -> Expression
apply (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpApply a b

match :: MkOpts -> Expression -> [Case]-> Expression
match (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpMatch a b

try :: MkOpts -> Expression -> [Case] -> Expression
try (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpTry a b

tuple :: MkOpts -> [Expression] -> Expression
tuple (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpTuple a

construct :: MkOpts -> Loc Longident -> Maybe Expression -> Expression
construct (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpConstruct a b

variant :: MkOpts -> Label -> Maybe Expression -> Expression
variant (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpVariant a b

record :: MkOpts -> [(Loc Longident, Expression)] -> Maybe Expression -> Expression
record (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpRecord a b

field :: MkOpts -> Expression -> Loc Longident -> Expression
field (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpField a b

setField :: MkOpts -> Expression -> Loc Longident -> Expression -> Expression
setField (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpSetField a b c

array :: MkOpts -> [Expression] -> Expression
array (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpArray a

ifThenElse :: MkOpts -> Expression -> Expression -> Maybe Expression -> Expression
ifThenElse (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpIfThenElse a b c

sequence :: MkOpts -> Expression -> Expression -> Expression
sequence (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpSequence a b

while :: MkOpts -> Expression -> Expression -> Expression
while (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpWhile a b

for :: MkOpts -> Pattern -> Expression -> Expression -> DirectionFlag -> Expression -> Expression
for (MkOpts {..}) a b c d e = mk (def { loc, attrs }) $ PexpFor a b c d e

constraint :: MkOpts -> Expression -> CoreType -> Expression
constraint (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpConstraint a b

coerce :: MkOpts -> Expression -> Maybe CoreType -> CoreType -> Expression
coerce (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpCoerce a b c

send :: MkOpts -> Expression -> Loc Label -> Expression
send (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpSend a b

new :: MkOpts -> Loc Longident -> Expression
new (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpNew a

setInstVar :: MkOpts -> Loc Label -> Expression -> Expression
setInstVar (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpSetInstVar a b

override :: MkOpts -> [(Loc Label, Expression)] -> Expression
override (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpOverride a

letModule :: MkOpts -> Loc String -> ModuleExpr -> Expression -> Expression
letModule (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpLetModule a b c

letException :: MkOpts -> ExtensionConstructor -> Expression -> Expression
letException (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpLetException a b

assert :: MkOpts -> Expression -> Expression
assert (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpAssert a

lazy :: MkOpts -> Expression -> Expression
lazy (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpLazy a

poly :: MkOpts -> Expression -> Maybe CoreType -> Expression
poly (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpPoly a b

object :: MkOpts -> ClassStructure -> Expression
object (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpObject a

newType :: MkOpts -> Loc String -> Expression -> Expression
newType (MkOpts {..}) a b = mk (def { loc, attrs }) $ PexpNewType a b

pack :: MkOpts -> ModuleExpr -> Expression
pack (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpPack a

open :: MkOpts -> OverrideFlag -> Loc Longident -> Expression -> Expression
open (MkOpts {..}) a b c = mk (def { loc, attrs }) $ PexpOpen a b c

extension :: MkOpts -> Extension -> Expression
extension (MkOpts {..}) a = mk (def { loc, attrs }) $ PexpExtension a

unreachable :: MkOpts -> () -> Expression
unreachable (MkOpts {..}) () = mk (def { loc, attrs }) PexpUnreachable
