{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Common
  ( addlb,
    expOfLabel,
    exprOfLetBindings,
    extraRHSCoreType,
    extraStr,
    extraText,
    ghexp,
    ghtyp,
    ghpat,
    mkexp,
    mkexpAttrs,
    mkexpCons,
    mkexpConstraint,
    mkexpOptConstraint,
    mkOperator,
    mkpatOptConstraint,
    mkinfix,
    mkLoc,
    mklb,
    mklbs,
    mkmod,
    mkmty,
    mkNewTypes,
    mkpat,
    mkpatAttrs,
    mkpatCons,
    mkpatvar,
    mkRHS,
    mksigExt,
    mkstr,
    mkstrExt,
    mkstrexp,
    mktailexp,
    mktailpat,
    mktyp,
    packageTypeOfModuleType,
    patOfLabel,
    relocExp,
    relocPat,
    rhsLoc,
    symbolDocs,
    symbolRLoc,
    textSig,
    textStr,
    valOfLetBindings,
    wrapExpAttrs,
    wrapTypeAnnotation,
  )
where

import Control.Eff (run)
import Control.Eff.Exception (runError)
import Data.Default (Default (..))
import Data.Maybe (Maybe (..), fromMaybe)
import Language.OCaml.Definitions.Parsing.ASTHelper.Exp as Exp
  ( MkOpts (loc),
    mk,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Mod as Mod
  ( MkOpts (attrs, loc),
    mk,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Mty as Mty
  ( MkOpts (attrs, loc),
    mk,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Pat as Pat
  ( MkOpts (loc),
    mk,
  )
import qualified Language.OCaml.Definitions.Parsing.ASTHelper.Sig as Sig
import qualified Language.OCaml.Definitions.Parsing.ASTHelper.Str as Str
import Language.OCaml.Definitions.Parsing.ASTHelper.Typ as Typ
  ( mk,
    varifyConstructors,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Vb as Vb
  ( MkOpts (attrs, docs, loc, text),
    mk,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( ArgLabel (Nolabel),
    Loc (..),
    RecFlag,
  )
import Language.OCaml.Definitions.Parsing.Docstrings
  ( Docs,
    Text,
    emptyDocs,
    rhsPostExtraText,
    rhsPostText,
    rhsPreExtraText,
    rhsText,
  )
import Language.OCaml.Definitions.Parsing.Location
  ( Location (..),
    none,
  )
import Language.OCaml.Definitions.Parsing.Longident as Longident
  ( Longident (Lident),
    last,
  )
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    Attributes,
    CoreType,
    CoreTypeDesc (PtypPoly),
    Expression (pexpAttributes, pexpLoc),
    ExpressionDesc
      ( PexpApply,
        PexpCoerce,
        PexpConstraint,
        PexpConstruct,
        PexpExtension,
        PexpIdent,
        PexpLet,
        PexpNewType,
        PexpTuple
      ),
    Longident (Lident),
    ModuleExpr,
    ModuleExprDesc,
    ModuleType (ModuleType, pmtyDesc),
    ModuleTypeDesc (PmtyIdent, PmtyWith),
    PackageType,
    Pattern (ppatAttributes, ppatLoc),
    PatternDesc
      ( PpatConstraint,
        PpatConstruct,
        PpatExtension,
        PpatTuple,
        PpatVar
      ),
    Payload (PPat, PSig, PStr),
    PrivateFlag (Public),
    SignatureItem,
    SignatureItemDesc (PsigExtension),
    StructureItem (..),
    StructureItemDesc (PstrEval, PstrExtension, PstrValue),
    TypeDeclaration
      ( TypeDeclaration,
        ptypeAttributes,
        ptypeCstrs,
        ptypeKind,
        ptypeManifest,
        ptypeParams,
        ptypePrivate
      ),
    TypeKind (PtypeAbstract),
    WithConstraint (PwithType),
    none,
  )
import Language.OCaml.Definitions.Parsing.Parser.LetBinding
  ( LetBinding (..),
  )
import Language.OCaml.Definitions.Parsing.Parser.LetBindings
  ( LetBindings (..),
  )
import Prelude hiding (exp, id)

mkstr :: StructureItemDesc -> StructureItem
mkstr = Str.mk def -- FIXME: symbolRLoc

ghstr :: StructureItemDesc -> StructureItem
ghstr = Str.mk def -- FIXME: symbol_gloc

wrapStrExt :: StructureItem -> Maybe (Loc String) -> StructureItem
wrapStrExt body ext = case ext of
  Nothing -> body
  Just i -> ghstr $ PstrExtension (i, PStr [body]) []

mkstrExt :: StructureItemDesc -> Maybe (Loc String) -> StructureItem
mkstrExt d = wrapStrExt (mkstr d)

mkstrexp :: Expression -> Attributes -> StructureItem
mkstrexp e attrs =
  StructureItem
    { pstrDesc = PstrEval e attrs,
      pstrLoc = pexpLoc e
    }

mkLoc :: a -> Location -> Loc a
mkLoc t l =
  Loc
    { txt = t,
      loc = l
    }

rhsLoc :: Int -> Location
rhsLoc _ = none -- FIXME

mkRHS :: a -> Int -> Loc a
mkRHS rhs pos = mkLoc rhs (rhsLoc pos)

mkmod :: Maybe [Attribute] -> ModuleExprDesc -> ModuleExpr
mkmod attrs' = Mod.mk
    ( def
        { loc = symbolRLoc (),
          attrs = fromMaybe (Mod.attrs def) attrs'
        }
    )

mkpat :: PatternDesc -> Pattern
mkpat = Pat.mk def -- FIXME

mkpatvar :: String -> Int -> Pattern
mkpatvar name pos = Pat.mk (def {loc = rhsLoc pos}) (PpatVar (mkRHS name pos))

mkpatCons :: Location -> Pattern -> Location -> Pattern
mkpatCons consloc args loc =
  Pat.mk (def {loc}) (PpatConstruct (mkLoc (Lident "::") consloc) (Just args))

mklb :: t -> (Pattern, Expression) -> Attributes -> LetBinding
mklb _first (p, e) attrs =
  LetBinding
    { lbPattern = p,
      lbExpression = e,
      lbAttributes = attrs,
      lbDocs = emptyDocs, -- symbolDocs_lazy ()
      lbText = [], --if first then empty_text_lazy
      --else symbol_text_lazy ()
      lbLoc = none -- symbolRLoc ()
    }

mklbs :: Maybe (Loc String) -> RecFlag -> LetBinding -> LetBindings
mklbs ext rf lb =
  LetBindings
    { lbsBindings = [lb],
      lbsRec = rf,
      lbsExtension = ext,
      lbsLoc = none -- symbolRLoc ()
    }

addlb :: LetBindings -> LetBinding -> LetBindings
addlb lbs lb =
  lbs {lbsBindings = lb : lbsBindings lbs}

valOfLetBindings :: LetBindings -> StructureItem
valOfLetBindings lbs =
  let bindings =
        map
          ( \lb ->
              Vb.mk
                ( def
                    { loc = lbLoc lb,
                      attrs = lbAttributes lb,
                      docs = lbDocs lb,
                      Vb.text = lbText lb -- weird, DisambiguateRecordFields should know?
                    }
                )
                (lbPattern lb)
                (lbExpression lb)
          )
          (lbsBindings lbs)
   in let str = mkstr $ PstrValue (lbsRec lbs) (reverse bindings)
       in case lbsExtension lbs of
            Nothing -> str
            Just i -> ghstr $ PstrExtension (i, PStr [str]) []

exprOfLetBindings :: LetBindings -> Expression -> Expression
exprOfLetBindings lbs body =
  let bindings =
        map
          ( \lb ->
              Vb.mk
                ( def
                    { loc = lbLoc lb,
                      attrs = lbAttributes lb
                    }
                )
                (lbPattern lb)
                (lbExpression lb)
          )
          (lbsBindings lbs)
   in mkexpAttrs (PexpLet (lbsRec lbs) (reverse bindings) body) (lbsExtension lbs, [])

wrapExpAttrs :: Expression -> (Maybe (Loc String), [Attribute]) -> Expression
wrapExpAttrs body (ext, attrs) =
  let body1 = body {pexpAttributes = attrs ++ pexpAttributes body}
   in case ext of
        Nothing -> body1
        Just i -> ghexp $ PexpExtension (i, PStr [mkstrexp body1 []])

mkexpAttrs :: ExpressionDesc -> (Maybe (Loc String), [Attribute]) -> Expression
mkexpAttrs d = wrapExpAttrs (mkexp d)

mkexp :: ExpressionDesc -> Expression
mkexp = Exp.mk def -- FIXME

mktyp :: CoreTypeDesc -> CoreType
mktyp = Typ.mk def -- FIXME

ghexp :: ExpressionDesc -> Expression
ghexp = Exp.mk def -- FIXME

ghpat :: PatternDesc -> Pattern
ghpat = Pat.mk def -- FIXME

ghtyp :: CoreTypeDesc -> CoreType
ghtyp = Typ.mk def -- FIXME

ghsig :: SignatureItemDesc -> SignatureItem
ghsig = Sig.mk def -- FIXME

mkexpConstraint :: Expression -> (Maybe CoreType, Maybe CoreType) -> Expression
mkexpConstraint e (t1, t2) = case (t1, t2) of
  (Just t, Nothing) -> ghexp $ PexpConstraint e t
  (_, Just t) -> ghexp $ PexpCoerce e t1 t
  (Nothing, Nothing) -> error "This should not happen"

relocExp :: Expression -> Expression
relocExp e = e {pexpLoc = none} -- FIXME

extraRHSCoreType :: CoreType -> a -> CoreType
extraRHSCoreType ct _pos =
  -- FIXME
  -- let docs = rhsInfo pos in
  ct -- { ptypAttributes = addInfoAttrs docs (ptypAttributes ct) }

symbolRLoc :: () -> Location
symbolRLoc () = none -- FIXME

relocPat :: Pattern -> Pattern
relocPat e = e {ppatLoc = symbolRLoc ()}

mkinfix :: Expression -> String -> Expression -> Expression
mkinfix arg1 name arg2 =
  mkexp $ PexpApply (mkoperator name 2) [(Nolabel, arg1), (Nolabel, arg2)]

mkoperator :: String -> Int -> Expression
mkoperator name pos =
  let loc = rhsLoc pos
   in Exp.mk (def {loc}) $ PexpIdent (mkLoc (Lident name) loc)

mktailexp :: Location -> [Expression] -> Expression
mktailexp nilloc = \case
  [] ->
    let loc = nilloc {locGhost = True}
     in let nil = Loc {txt = Lident "[]", loc}
         in Exp.mk (def {loc}) $ PexpConstruct nil Nothing
  e1 : el ->
    let expEl = mktailexp nilloc el
     in let loc =
              Location
                { locStart = locStart . pexpLoc $ e1,
                  locEnd = locEnd . pexpLoc $ e1,
                  locGhost = True
                }
         in let arg = Exp.mk (def {loc}) $ PexpTuple [e1, expEl]
             in mkexpCons (loc {locGhost = True}) arg loc

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
    let loc = nilloc {locGhost = True}
     in let nil = Loc {txt = Lident "[]", loc}
         in Pat.mk (def {loc}) $ PpatConstruct nil Nothing
  p1 : pl ->
    let patPl = mktailpat nilloc pl
     in let loc =
              Location
                { locStart = locStart $ ppatLoc p1,
                  locEnd = locEnd $ ppatLoc patPl,
                  locGhost = True
                }
         in let arg = Pat.mk (def {loc}) $ PpatTuple [p1, patPl]
             in mkpatCons (loc {locGhost = True}) arg loc

symbolDocs :: () -> Docs
symbolDocs () = emptyDocs -- FIXME

textStr :: Int -> [StructureItem]
textStr pos = Str.text (rhsText pos)

textSig :: Int -> [SignatureItem]
textSig pos = Sig.text (rhsText pos)

extraText :: (Text -> [a]) -> p -> [a] -> [a]
extraText text pos [] =
  let post = rhsPostText pos
   in let postExtras = rhsPostExtraText pos
       in text post ++ text postExtras
extraText text pos items =
  let preExtras = rhsPreExtraText pos
   in let postExtras = rhsPostExtraText pos
       in text preExtras ++ items ++ text postExtras

extraStr :: p -> [StructureItem] -> [StructureItem]
extraStr = extraText Str.text

mkNewTypes :: [Loc String] -> Expression -> Expression
mkNewTypes =
  flip $ foldr (\newType exp -> mkexp $ PexpNewType newType exp)

wrapTypeAnnotation :: [Loc String] -> CoreType -> Expression -> (Expression, CoreType)
wrapTypeAnnotation newTypes coreType body =
  let exp1 = mkexp $ PexpConstraint body coreType
   in let exp2 = mkNewTypes newTypes exp1
       in -- handling side-effects locally for now
          let varC = case run . runError $ Typ.varifyConstructors newTypes coreType of
                Left e -> error e
                Right r -> r
           in (exp2, ghtyp $ PtypPoly newTypes varC)

mkmty :: AttrsOpts -> ModuleTypeDesc -> ModuleType
mkmty (AttrsOpts {..}) = Mty.mk (def {Mty.attrs = attrs, loc = symbolRLoc ()})

newtype AttrsOpts = AttrsOpts {attrs :: Attributes}

instance Default AttrsOpts where
  def = AttrsOpts []

packageTypeOfModuleType :: ModuleType -> PackageType
packageTypeOfModuleType ModuleType {pmtyDesc = PmtyIdent lid} = (lid, [])
packageTypeOfModuleType ModuleType {pmtyDesc = PmtyWith (ModuleType {pmtyDesc = PmtyIdent lid}) cstrs} =
  (lid, map mapCstr cstrs)
  where
    mapCstr :: WithConstraint -> (Loc Longident, CoreType)
    mapCstr = \case
      PwithType lid' ptyp ->
        case ptyp of
          TypeDeclaration {ptypeParams = []} -> error "TODO"
          TypeDeclaration {ptypeCstrs = []} -> error "TODO"
          TypeDeclaration {ptypePrivate} | ptypePrivate /= Public -> error "TODO"
          TypeDeclaration {ptypeKind} | ptypeKind /= PtypeAbstract -> error "TODO"
          TypeDeclaration {ptypeAttributes} | ptypeAttributes /= [] -> error "TODO"
          TypeDeclaration {ptypeManifest} -> case ptypeManifest of
            Just ty -> (lid', ty)
            Nothing -> error "This should not happen"
      _ -> error "Only 'with type t =' constraints are supported"
packageTypeOfModuleType _ = error "TODO"

mkpatAttrs :: PatternDesc -> (Maybe (Loc String), [Attribute]) -> Pattern
mkpatAttrs d = wrapPatAttrs (mkpat d)

wrapPatAttrs :: Pattern -> (Maybe (Loc String), [Attribute]) -> Pattern
wrapPatAttrs pat0 (ext, attrs) =
  let pat = pat0 {ppatAttributes = attrs ++ ppatAttributes pat0}
   in case ext of
        Nothing -> pat
        Just id' -> ghpat $ PpatExtension (id', PPat pat Nothing)

mkexpCons :: Location -> Expression -> Location -> Expression
mkexpCons consLoc args loc =
  Exp.mk (def {loc}) $ PexpConstruct (mkLoc (Lident "::") consLoc) (Just args)

expOfLabel :: Longident -> Int -> Expression
expOfLabel lbl pos = mkexp $ PexpIdent (mkRHS (Lident (Longident.last lbl)) pos)

mkOperator :: String -> Int -> Expression
mkOperator name pos =
  let loc = rhsLoc pos
   in Exp.mk (def {loc}) $ PexpIdent (mkLoc (Lident name) loc)

wrapSigExt :: SignatureItem -> Maybe (Loc String) -> SignatureItem
wrapSigExt body ext =
  case ext of
    Nothing -> body
    Just id -> ghsig $ PsigExtension (id, PSig [body]) []

mksig :: SignatureItemDesc -> SignatureItem
mksig = Sig.mk def -- FIXME

mksigExt :: SignatureItemDesc -> Maybe (Loc String) -> SignatureItem
mksigExt d = wrapSigExt (mksig d)
