{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Common
  ( addlb
  , expOfLabel
  , exprOfLetBindings
  , extraRHSCoreType
  , extraStr
  , extraText
  , ghexp
  , ghtyp
  , ghpat
  , identP
  , mkexp
  , mkexpAttrs
  , mkexpCons
  , mkexpConstraint
  , mkexpOptConstraint
  , mkOperator
  , mkpatOptConstraint
  , mkinfix
  , mkLoc
  , mklb
  , mklbs
  , mkmod
  , mkmty
  , mkNewTypes
  , mkpat
  , mkpatAttrs
  , mkpatCons
  , mkpatvar
  , mkRHS
  , mkstr
  , mkstrExt
  , mkstrexp
  , mktailexp
  , mktailpat
  , mktyp
  , packageTypeOfModuleType
  , patOfLabel
  , relocExp
  , relocPat
  , rhsLoc
  , symbolDocs
  , symbolRLoc
  , textStr
  , valOfLetBindings
  , wrapExpAttrs
  , wrapTypeAnnotation
  ) where

import           Control.Eff
import           Control.Eff.Exception
import           Data.Default
import           Data.Maybe
import           Prelude hiding (exp)
import           Text.Megaparsec

import           Language.OCaml.Definitions.Parsing.ASTHelper.Exp as Exp hiding (newType)
import           Language.OCaml.Definitions.Parsing.ASTHelper.Mod as Mod
import           Language.OCaml.Definitions.Parsing.ASTHelper.Mty as Mty
import           Language.OCaml.Definitions.Parsing.ASTHelper.Pat as Pat
import           Language.OCaml.Definitions.Parsing.ASTHelper.Str as Str hiding (text)
import qualified Language.OCaml.Definitions.Parsing.ASTHelper.Str as Str (text)
import           Language.OCaml.Definitions.Parsing.ASTHelper.Typ as Typ
import           Language.OCaml.Definitions.Parsing.ASTHelper.Vb as Vb
import           Language.OCaml.Definitions.Parsing.ASTTypes
import           Language.OCaml.Definitions.Parsing.Docstrings
import           Language.OCaml.Definitions.Parsing.Location
import           Language.OCaml.Definitions.Parsing.Longident as Longident
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Definitions.Parsing.Parser.LetBinding
import           Language.OCaml.Definitions.Parsing.Parser.LetBindings
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Utils
import           Language.OCaml.Parser.Utils.Types

identP :: Parser String
identP = lexeme $ choice [ uIdentT, lIdentT ]

mkstr :: StructureItemDesc -> StructureItem
mkstr d = Str.mk def d -- FIXME: symbolRLoc

ghstr :: StructureItemDesc -> StructureItem
ghstr d = Str.mk def d -- FIXME: symbol_gloc

wrapStrExt :: StructureItem -> Maybe (Loc String) -> StructureItem
wrapStrExt body ext = case ext of
  Nothing -> body
  Just i -> ghstr $ PstrExtension (i, PStr [body]) []

mkstrExt :: StructureItemDesc -> Maybe (Loc String) -> StructureItem
mkstrExt d ext = wrapStrExt (mkstr d) ext

mkstrexp :: Expression -> Attributes -> StructureItem
mkstrexp e attrs = StructureItem
  { pstrDesc = PstrEval e attrs
  , pstrLoc = pexpLoc e
  }

mkLoc :: a -> Location -> Loc a
mkLoc t l = Loc
  { txt = t
  , loc = l
  }

rhsLoc :: Int -> Location
rhsLoc _ = none -- FIXME

mkRHS :: a -> Int -> Loc a
mkRHS rhs pos = mkLoc rhs (rhsLoc pos)

mkmod :: Maybe [Attribute] -> ModuleExprDesc -> ModuleExpr
mkmod attrs' d =
  Mod.mk (def { loc   = symbolRLoc ()
              , attrs = fromMaybe (Mod.attrs def) attrs'
              }
         ) d

mkpat :: PatternDesc -> Pattern
mkpat d = Pat.mk def d -- FIXME

mkpatvar :: String -> Int -> Pattern
mkpatvar name pos = Pat.mk (def { loc = rhsLoc pos }) (PpatVar (mkRHS name pos))

mkpatCons :: Location -> Pattern -> Location -> Pattern
mkpatCons consloc args loc =
  Pat.mk (def { loc }) (PpatConstruct (mkLoc (Lident "::") consloc) (Just args))

mklb :: t -> (Pattern, Expression) -> Attributes -> LetBinding
mklb _first (p, e) attrs = LetBinding
  { lbPattern    = p
  , lbExpression = e
  , lbAttributes = attrs
  , lbDocs       = emptyDocs -- symbolDocs_lazy ()
  , lbText       = [] --if first then empty_text_lazy
                    --else symbol_text_lazy ()
  , lbLoc        = none -- symbolRLoc ()
  }

mklbs :: Maybe (Loc String) -> RecFlag -> LetBinding -> LetBindings
mklbs ext rf lb = LetBindings
  { lbsBindings  = [lb]
  , lbsRec       = rf
  , lbsExtension = ext
  , lbsLoc       = none -- symbolRLoc ()
  }

addlb :: LetBindings -> LetBinding -> LetBindings
addlb lbs lb =
  lbs { lbsBindings = lb : lbsBindings lbs }

valOfLetBindings :: LetBindings -> StructureItem
valOfLetBindings lbs =
  let bindings =
        map
        (\ lb -> Vb.mk
                 (def { loc     = lbLoc lb
                      , attrs   = lbAttributes lb
                      , docs    = lbDocs lb
                      , Vb.text = lbText lb -- weird, DisambiguateRecordFields should know?
                      }
                 )
                 (lbPattern lb)
                 (lbExpression lb)
        )
        (lbsBindings lbs)
  in
  let str = mkstr $ PstrValue (lbsRec lbs) (reverse bindings) in
  case lbsExtension lbs of
    Nothing -> str
    Just i  -> ghstr $ PstrExtension (i, PStr [str]) []

exprOfLetBindings :: LetBindings -> Expression -> Expression
exprOfLetBindings lbs body =
  let bindings =
        map
        (\ lb -> Vb.mk
                 (def { loc   = lbLoc lb
                      , attrs = lbAttributes lb
                      }
                 )
                 (lbPattern lb)
                 (lbExpression lb)
        )
        (lbsBindings lbs)
  in
  mkexpAttrs (PexpLet (lbsRec lbs) (reverse bindings) body) (lbsExtension lbs, [])

wrapExpAttrs :: Expression -> (Maybe (Loc String), [Attribute]) -> Expression
wrapExpAttrs body (ext, attrs) =
  let body1 = body { pexpAttributes = attrs ++ pexpAttributes body } in
  case ext of
    Nothing -> body1
    Just i  -> ghexp $ PexpExtension (i, (PStr [mkstrexp body1 []]))

mkexpAttrs :: ExpressionDesc -> (Maybe (Loc String), [Attribute]) -> Expression
mkexpAttrs d attrs = wrapExpAttrs (mkexp d) attrs

mkexp :: ExpressionDesc -> Expression
mkexp d = Exp.mk def d -- FIXME

mktyp :: CoreTypeDesc -> CoreType
mktyp d = Typ.mk def d -- FIXME

ghexp :: ExpressionDesc -> Expression
ghexp d = Exp.mk def d -- FIXME

ghpat :: PatternDesc -> Pattern
ghpat d = Pat.mk def d -- FIXME

ghtyp :: CoreTypeDesc -> CoreType
ghtyp d = Typ.mk def d -- FIXME

mkexpConstraint :: Expression -> (Maybe CoreType, Maybe CoreType) -> Expression
mkexpConstraint e (t1, t2) = case (t1, t2) of
  (Just t,  Nothing) -> ghexp $ PexpConstraint e t
  (_,       Just t)  -> ghexp $ PexpCoerce e t1 t
  (Nothing, Nothing) -> error "This should not happen"

relocExp :: Expression -> Expression
relocExp e = e { pexpLoc = none } -- FIXME

extraRHSCoreType :: CoreType -> a -> CoreType
extraRHSCoreType ct _pos =
  -- FIXME
  -- let docs = rhsInfo pos in
  ct -- { ptypAttributes = addInfoAttrs docs (ptypAttributes ct) }

symbolRLoc :: () -> Location
symbolRLoc () = none -- FIXME

relocPat :: Pattern -> Pattern
relocPat e = e { ppatLoc = symbolRLoc () }

mkinfix :: Expression -> String -> Expression -> Expression
mkinfix arg1 name arg2 =
  mkexp $ PexpApply (mkoperator name 2) [(Nolabel, arg1), (Nolabel, arg2)]

mkoperator :: String -> Int -> Expression
mkoperator name pos =
  let loc = rhsLoc pos in
  Exp.mk (def { loc }) $ PexpIdent (mkLoc (Lident name) loc)

mktailexp :: Location -> [Expression] -> Expression
mktailexp nilloc = \case
  [] ->
    let loc = nilloc { locGhost = True } in
    let nil = Loc { txt = Lident "[]", loc } in
    Exp.mk (def { loc }) $ PexpConstruct nil Nothing
  e1 : el ->
    let expEl = mktailexp nilloc el in
    let loc = Location
          { locStart = locStart . pexpLoc $ e1
          , locEnd   = locEnd   . pexpLoc $ e1
          , locGhost = True
          }
    in
    let arg = Exp.mk (def { loc }) $ PexpTuple [e1, expEl] in
    mkexpCons (loc {locGhost = True}) arg loc

mkexpOptConstraint :: Expression -> Maybe (Maybe CoreType, Maybe CoreType) -> Expression
mkexpOptConstraint e = \case
  Nothing -> e
  Just constraint' -> mkexpConstraint e constraint'

mkpatOptConstraint :: Pattern -> Maybe CoreType -> Pattern
mkpatOptConstraint p = \case
  Nothing -> p
  Just typ -> mkpat (PpatConstraint p typ)

patOfLabel :: Longident -> Int -> Pattern
patOfLabel lbl pos = mkpat $ PpatVar $ mkRHS (Longident.last lbl) pos

mktailpat :: Location -> [Pattern] -> Pattern
mktailpat nilloc = \case
  [] ->
    let loc = nilloc { locGhost = True } in
    let nil = Loc { txt = Lident "[]", loc } in
    Pat.mk (def { loc }) $ PpatConstruct nil Nothing
  p1 : pl ->
    let patPl = mktailpat nilloc pl in
    let loc = Location
          { locStart = locStart $ ppatLoc p1
          , locEnd   = locEnd   $ ppatLoc patPl
          , locGhost = True
          }
    in
    let arg = Pat.mk (def { loc }) $ PpatTuple [p1, patPl] in
    mkpatCons (loc { locGhost = True }) arg loc

symbolDocs :: () -> Docs
symbolDocs () = emptyDocs -- FIXME

textStr :: Int -> [StructureItem]
textStr pos = Str.text (rhsText pos)

extraText :: (Text -> [a]) -> p -> [a] -> [a]
extraText text pos []    = let post = rhsPostText pos in
                           let postExtras = rhsPostExtraText pos in
                           text post ++ text postExtras
extraText text pos items = let preExtras = rhsPreExtraText pos in
                           let postExtras = rhsPostExtraText pos in
                           text preExtras ++ items ++ text postExtras

extraStr :: p -> [StructureItem] -> [StructureItem]
extraStr pos items = extraText Str.text pos items

mkNewTypes :: [Loc String] -> Expression -> Expression
mkNewTypes =
  flip $ foldr (\ newType exp -> mkexp $ PexpNewType newType exp)

wrapTypeAnnotation :: [Loc String] -> CoreType -> Expression -> (Expression, CoreType)
wrapTypeAnnotation newTypes coreType body =
  let exp1 = mkexp $ PexpConstraint body coreType in
  let exp2 = mkNewTypes newTypes exp1 in
  -- handling side-effects locally for now
  let varC = case run . runError $ Typ.varifyConstructors newTypes coreType of
        Left  e -> error e
        Right r -> r
  in
  (exp2, ghtyp $ PtypPoly newTypes varC)

mkmty :: AttrsOpts -> ModuleTypeDesc -> ModuleType
mkmty (AttrsOpts {..}) d = Mty.mk (def { Mty.attrs = attrs, loc = symbolRLoc () }) d

data AttrsOpts = AttrsOpts { attrs :: Attributes }

instance Default AttrsOpts where
  def = AttrsOpts []

packageTypeOfModuleType :: ModuleType -> PackageType
packageTypeOfModuleType (ModuleType { pmtyDesc = PmtyIdent lid }) = (lid, [])
packageTypeOfModuleType (ModuleType { pmtyDesc = PmtyWith (ModuleType { pmtyDesc = PmtyIdent lid }) cstrs}) =
  (lid, map mapCstr cstrs)
  where
    mapCstr :: WithConstraint -> (Loc Longident, CoreType)
    mapCstr = \case
      PwithType lid' ptyp ->
        case ptyp of
          TypeDeclaration { ptypeParams = [] } -> error "TODO"
          TypeDeclaration { ptypeCstrs = [] } -> error "TODO"
          TypeDeclaration { ptypePrivate }    | ptypePrivate    /= Public        -> error "TODO"
          TypeDeclaration { ptypeKind }       | ptypeKind       /= PtypeAbstract -> error "TODO"
          TypeDeclaration { ptypeAttributes } | ptypeAttributes /= []            -> error "TODO"
          TypeDeclaration { ptypeManifest } -> case ptypeManifest of
            Just ty -> (lid', ty)
            Nothing -> error "This should not happen"
      _ -> error "Only 'with type t =' constraints are supported"
packageTypeOfModuleType _ = error "TODO"

mkpatAttrs :: PatternDesc -> (Maybe (Loc String), [Attribute]) -> Pattern
mkpatAttrs d attrs = wrapPatAttrs (mkpat d) attrs

wrapPatAttrs :: Pattern -> (Maybe (Loc String), [Attribute]) -> Pattern
wrapPatAttrs pat0 (ext, attrs) =
  let pat = pat0 { ppatAttributes = attrs ++ ppatAttributes pat0 } in
  case ext of
  Nothing -> pat
  Just id' -> ghpat $ PpatExtension (id', PPat pat Nothing)

mkexpCons :: Location -> Expression -> Location -> Expression
mkexpCons consLoc args loc =
  Exp.mk (def { loc }) $ PexpConstruct (mkLoc (Lident "::") consLoc) (Just args)

expOfLabel :: Longident -> Int -> Expression
expOfLabel lbl pos = mkexp $ PexpIdent (mkRHS (Lident (Longident.last lbl)) pos)

mkOperator :: String -> Int -> Expression
mkOperator name pos =
  let loc = rhsLoc pos in
  Exp.mk (def { loc }) $ PexpIdent (mkLoc (Lident name) loc)
