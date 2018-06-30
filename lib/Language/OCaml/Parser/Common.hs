{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Common
  ( CaseExpOpts(..)
  , ConstructorOpts(..)
  , DeclOpts(..)
  , MkExceptionOpts(..)
  , MkTypeOpts(..)
  , addlb
  , caseExp
  , constructor
  , decl
  , exprOfLetBindings
  , extraRHSCoreType
  , ghexp
  , ghpat
  , ghtyp
  , identP
  , mkException
  , mkExp
  , mkexp
  , mkexpAttrs
  , mkexpConstraint
  , mkexpOptConstraint
  , mkpatOptConstraint
  , mkinfix
  , mkLoc
  , mklb
  , mklbs
  , mkMb
  , mkMod
  , mkmod
  , mkOpn
  , mkPat
  , mkpat
  , mkpatCons
  , mkpatvar
  , mkRHS
  , mkStr
  , mkstr
  , mkstrExt
  , mkstrexp
  , mktailexp
  , mktailpat
  , mkTe
  , mkTyp
  , mkType
  , mktyp
  , mkVb
  , patOfLabel
  , rebind
  , relocExp
  , relocPat
  , rhsLoc
  , symbolRLoc
  , textStr
  , valOfLetBindings
  ) where

import Data.Default
import Data.Maybe
import Text.Megaparsec

import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.Docstrings
import Language.OCaml.Definitions.Parsing.Location
import Language.OCaml.Definitions.Parsing.Longident as Longident
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Definitions.Parsing.Parser.LetBinding
import Language.OCaml.Definitions.Parsing.Parser.LetBindings
import Language.OCaml.Parser.Tokens
import Language.OCaml.Parser.Utils.Utils
import Language.OCaml.Parser.Utils.Types

defaultLoc :: Location
defaultLoc = none

identP :: Parser String
identP = lexeme $ choice [ uIdentT, lIdentT ]

mkStr :: Maybe Location -> StructureItemDesc -> StructureItem
mkStr l d =
  StructureItem
  { pstrDesc = d
  , pstrLoc  = fromMaybe defaultLoc l
  }

mkstr :: StructureItemDesc -> StructureItem
mkstr d = mkStr Nothing d -- FIXME: symbolRLoc

ghstr :: StructureItemDesc -> StructureItem
ghstr d = mkStr Nothing d -- FIXME: symbol_gloc

wrapStrExt :: StructureItem -> Maybe (Loc String) -> StructureItem
wrapStrExt body ext = case ext of
  Nothing -> body
  Just i -> ghstr $ PstrExtension (i, PStr [body]) []

mkstrExt :: StructureItemDesc -> Maybe (Loc String) -> StructureItem
mkstrExt d ext = wrapStrExt (mkstr d) ext

mkTyp :: MkTypOpts -> CoreTypeDesc -> CoreType
mkTyp (MkTypOpts {..}) desc =
  CoreType
  { ptypDesc       = desc
  , ptypLoc        = loc
  , ptypAttributes = attrs
  }

data MkTypOpts = MkTypOpts
  { loc   :: Location
  , attrs :: [Attribute]
  }

instance Default MkTypOpts where
  def = MkTypOpts
    { loc   = defaultLoc
    , attrs = []
    }

mkExp :: MkExpOpts -> ExpressionDesc -> Expression
mkExp (MkExpOpts {..}) desc =
  Expression
  { pexpDesc       = desc
  , pexpLoc        = loc
  , pexpAttributes = attrs
  }

data MkExpOpts = MkExpOpts
  { attrs  :: [Attribute]
  , loc    :: Location
  }

instance Default MkExpOpts where
  def = MkExpOpts
    { attrs  = []
    , loc    = defaultLoc
    }

data CaseExpOpts = CaseExpOpts
  { guard :: Maybe Expression
  }

instance Default CaseExpOpts where
  def = CaseExpOpts
    { guard  = Nothing
    }

caseExp :: CaseExpOpts -> Pattern -> Expression -> Case
caseExp (CaseExpOpts {..}) lhs rhs = Case
  { pcLHS   = lhs
  , pcGuard = guard
  , pcRHS   = rhs
  }

data MkTypeOpts = MkTypeOpts
  { attrs  :: [Attribute]
  , docs   :: ()
  , cstrs  :: [(CoreType, CoreType, Location)]
  , kind   :: TypeKind
  , loc    :: Location
  , params :: [(CoreType, Variance)]
  , priv   :: PrivateFlag
  , text   :: ()
  }

instance Default MkTypeOpts where
  def = MkTypeOpts
    { attrs  = []
    , cstrs  = []
    , docs   = () -- FIXME
    , kind   = PtypeAbstract
    , loc    = defaultLoc
    , params = []
    , priv   = Public
    , text   = () -- FIXME
    }

mkType :: MkTypeOpts -> Maybe CoreType -> Loc String -> TypeDeclaration
mkType (MkTypeOpts {..}) manifest name =
  TypeDeclaration
  { ptypeName       = name
  , ptypeParams     = params
  , ptypeCstrs      = cstrs
  , ptypeKind        = kind
  , ptypePrivate    = priv
  , ptypeManifest   = manifest
  , ptypeAttributes = attrs
  , ptypeLoc        = loc
  }

mkOpn ::
  Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe OverrideFlag -> Loc Longident ->
  OpenDescription
mkOpn loc attrs docs override lid =
  OpenDescription
  { popenLid        = lid
  , popenOverride   = fromMaybe Fresh override
  , popenLoc        = fromMaybe defaultLoc loc
  , popenAttributes = addDocsAttrs (fromMaybe emptyDocs docs) (fromMaybe [] attrs)
  }

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

textStr :: Int -> [StructureItem]
textStr pos = textStrTODO (rhsText pos)

attributeStr :: Maybe Location -> Attribute -> StructureItem
attributeStr mloc a = mkStr mloc (PstrAttribute a)

textStrTODO :: [Docstring] -> [StructureItem]
textStrTODO text = map (\ ds -> attributeStr Nothing (textAttr ds)) text

mkmod :: Maybe [Attribute] -> ModuleExprDesc -> ModuleExpr
mkmod attrs d = mkMod Nothing attrs d -- FIXME: Nothing

mkMod :: Maybe Location -> Maybe [Attribute] -> ModuleExprDesc -> ModuleExpr
mkMod loc attrs d = ModuleExpr
  { pmodDesc       = d
  , pmodLoc        = fromMaybe defaultLoc loc
  , pmodAttributes = fromMaybe []          attrs
  }

mkMb ::
  Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe [Docstring] -> Loc String -> ModuleExpr ->
  ModuleBinding
mkMb loc attrs docs text name expr = ModuleBinding
  { pmbName       = name
  , pmbExpr       = expr
  , pmbAttributes = addTextAttrs (fromMaybe [] text)
                     . addDocsAttrs (fromMaybe emptyDocs docs)
                     $ fromMaybe [] attrs
  , pmbLoc        = fromMaybe defaultLoc loc
  }

mkPat :: MkPatOpts -> PatternDesc -> Pattern
mkPat (MkPatOpts {..}) d = Pattern
  { ppatDesc       = d
  , ppatLoc        = loc
  , ppatAttributes = attrs
  }

data MkPatOpts = MkPatOpts
  { attrs :: [Attribute]
  , loc   :: Location
  }

instance Default MkPatOpts where
  def = MkPatOpts
    { attrs = []
    , loc   = defaultLoc
    }

mkpat :: PatternDesc -> Pattern
mkpat d = mkPat def d -- FIXME

mkpatvar :: String -> Int -> Pattern
mkpatvar name pos = mkPat (def { loc = rhsLoc pos }) (PpatVar (mkRHS name pos))

mkpatCons :: Location -> Pattern -> Location -> Pattern
mkpatCons consloc args loc =
  mkPat (def { loc }) (PpatConstruct (mkLoc (Lident "::") consloc) (Just args))

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

mkVb :: Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe Text -> Pattern -> Expression -> ValueBinding
mkVb loc attrs docs text pat expr = ValueBinding
  { pvbPat        = pat
  , pvbExpr       = expr
  , pvbAttributes = addTextAttrs (fromMaybe [] text)
                     . addDocsAttrs (fromMaybe emptyDocs docs)
                     $ fromMaybe [] attrs
  , pvbLoc        = fromMaybe defaultLoc loc
  }

valOfLetBindings :: LetBindings -> StructureItem
valOfLetBindings lbs =
  let bindings =
        map
        (\ lb -> mkVb
                    (Just $ lbLoc lb)
                    (Just $ lbAttributes lb)
                    (Just $ lbDocs lb)
                    (Just $ lbText lb)
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
        (\ lb -> mkVb
                    (Just $ lbLoc lb)
                    (Just $ lbAttributes lb)
                    Nothing
                    Nothing
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
mkexp d = mkExp def d -- FIXME

mktyp :: CoreTypeDesc -> CoreType
mktyp d = mkTyp def d -- FIXME

ghexp :: ExpressionDesc -> Expression
ghexp d = mkExp def d -- FIXME

ghpat :: PatternDesc -> Pattern
ghpat d = mkPat def d -- FIXME

ghtyp :: CoreTypeDesc -> CoreType
ghtyp d = mkTyp def d -- FIXME

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

mkTe :: MkTeOpts -> Loc Longident -> [ExtensionConstructor] -> TypeExtension
mkTe (MkTeOpts {..}) path constructors = TypeExtension
  { ptyextPath         = path
  , ptyextParams       = params
  , ptyextConstructors = constructors
  , ptyextPrivate      = priv
  , ptyextAttributes   = attrs
  }

data MkTeOpts = MkTeOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , params :: [(CoreType, Variance)]
  , priv   :: PrivateFlag
  }

mkException :: MkExceptionOpts -> ExtensionConstructor -> TypeException
mkException (MkExceptionOpts {..}) ctor = TypeException
  { ptyexnConstructor = ctor
  , ptyexnAttributes  = attrs
  }

data MkExceptionOpts = MkExceptionOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  }

instance Default MkExceptionOpts where
  def = MkExceptionOpts
    { attrs  = []
    , docs   = emptyDocs
    }

rebind :: RebindOpts -> Loc String -> Loc Longident -> ExtensionConstructor
rebind (RebindOpts {..}) name lid = ExtensionConstructor
  { pextName       = name
  , pextKind       = PextRebind lid
  , pextLoc        = loc
  , pextAttributes = addDocsAttrs docs (addInfoAttrs info attrs)
  }

data RebindOpts = RebindOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , loc    :: Location
  , info   :: Info
  }

instance Default RebindOpts where
  def = RebindOpts
    { attrs  = []
    , docs   = emptyDocs
    , loc    = defaultLoc
    , info   = emptyInfo
    }

decl :: DeclOpts -> Maybe CoreType -> Loc String -> ExtensionConstructor
decl (DeclOpts {..}) res name = ExtensionConstructor
  { pextName       = name
  , pextKind       = PextDecl args res
  , pextLoc        = loc
  , pextAttributes = addDocsAttrs docs (addInfoAttrs info attrs)
  }

data DeclOpts = DeclOpts
  { args   :: ConstructorArguments
  , attrs  :: [Attribute]
  , docs   :: Docs
  , loc    :: Location
  , info   :: Info
  }

instance Default DeclOpts where
  def = DeclOpts
    { args   = PcstrTuple []
    , attrs  = []
    , docs   = emptyDocs
    , loc    = defaultLoc
    , info   = emptyInfo
    }

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
  mkExp (def { loc }) $ PexpIdent (mkLoc (Lident name) loc)

constructor ::
  ConstructorOpts ->
  Maybe CoreType ->
  Loc String ->
  ConstructorDeclaration
constructor (ConstructorOpts {..}) res name =
  ConstructorDeclaration
  { pcdName       = name
  , pcdArgs       = args
  , pcdRes        = res
  , pcdLoc        = loc
  , pcdAttributes = attrs
  }

data ConstructorOpts = ConstructorOpts
  { args   :: ConstructorArguments
  , attrs  :: [Attribute]
  , loc    :: Location
  , info   :: Info
  }

instance Default ConstructorOpts where
  def = ConstructorOpts
    { args = PcstrTuple []
    , attrs = []
    , loc   = defaultLoc
    , info  = emptyInfo
    }

mktailexp :: Location -> [Expression] -> Expression
mktailexp nilloc = \case
  [] ->
    let loc = nilloc { locGhost = True } in
    let nil = Loc { txt = Lident "[]", loc } in
    mkExp (def { loc }) $ PexpConstruct nil Nothing
  e1 : el ->
    let expEl = mktailexp nilloc el in
    let loc = Location
          { locStart = locStart . pexpLoc $ e1
          , locEnd   = locEnd   . pexpLoc $ e1
          , locGhost = True
          }
    in
    let arg = mkExp (def { loc }) $ PexpTuple [e1, expEl] in
    mkexpCons (loc {locGhost = True}) arg loc

mkexpCons :: Location -> Expression -> Location -> Expression
mkexpCons consloc args loc = mkExp (def { loc }) $ PexpConstruct (mkLoc (Lident "::") consloc) (Just args)

mkexpOptConstraint :: Expression -> Maybe (Maybe CoreType, Maybe CoreType) -> Expression
mkexpOptConstraint e = \case
  Nothing -> e
  Just constraint -> mkexpConstraint e constraint

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
    mkPat (def { loc }) $ PpatConstruct nil Nothing
  p1 : pl ->
    let patPl = mktailpat nilloc pl in
    let loc = Location
          { locStart = locStart $ ppatLoc p1
          , locEnd   = locEnd   $ ppatLoc patPl
          , locGhost = True
          }
    in
    let arg = mkPat (def { loc }) $ PpatTuple [p1, patPl] in
    mkpatCons (loc { locGhost = True }) arg loc
