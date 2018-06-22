{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Parser.Common
  ( ConstructorOpts(..)
  , DeclOpts(..)
  , MkExceptionOpts(..)
  , MkTypeOpts(..)
  , addlb
  , caseExp
  , constructor
  , decl
  , expr_of_let_bindings
  , extra_rhs_core_type
  , ghexp
  , ghpat
  , ghtyp
  , ident_P
  , mk_exception
  , mkExp
  , mkexp
  , mkexp_attrs
  , mkexp_constraint
  , mkexp_opt_constraint
  , mkpat_opt_constraint
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
  , mkpat_cons
  , mkpatvar
  , mkRHS
  , mkStr
  , mkstr
  , mkstr_ext
  , mkstrexp
  , mktailexp
  , mktailpat
  , mkTe
  , mkTyp
  , mkType
  , mktyp
  , mkVb
  , pat_of_label
  , rebind
  , reloc_exp
  , reloc_pat
  , rhsLoc
  , symbol_rloc
  , text_str
  , val_of_let_bindings
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

default_loc :: Location
default_loc = none

ident_P :: Parser String
ident_P = lexeme $ choice [ u_ident_T, l_ident_T ]

mkStr :: Maybe Location -> Structure_item_desc -> Structure_item
mkStr l d =
  Structure_item
  { pstr_desc = d
  , pstr_loc  = fromMaybe default_loc l
  }

mkstr :: Structure_item_desc -> Structure_item
mkstr d = mkStr Nothing d -- FIXME: symbol_rloc

ghstr :: Structure_item_desc -> Structure_item
ghstr d = mkStr Nothing d -- FIXME: symbol_gloc

wrap_str_ext :: Structure_item -> Maybe (Loc String) -> Structure_item
wrap_str_ext body ext = case ext of
  Nothing -> body
  Just i -> ghstr $ Pstr_extension (i, PStr [body]) []

mkstr_ext :: Structure_item_desc -> Maybe (Loc String) -> Structure_item
mkstr_ext d ext = wrap_str_ext (mkstr d) ext

mkTyp :: MkTypOpts -> Core_type_desc -> Core_type
mkTyp (MkTypOpts {..}) desc =
  Core_type
  { ptyp_desc       = desc
  , ptyp_loc        = loc
  , ptyp_attributes = attrs
  }

data MkTypOpts = MkTypOpts
  { loc   :: Location
  , attrs :: [Attribute]
  }

instance Default MkTypOpts where
  def = MkTypOpts
    { loc   = default_loc
    , attrs = []
    }

mkExp :: MkExpOpts -> Expression_desc -> Expression
mkExp (MkExpOpts {..}) desc =
  Expression
  { pexp_desc       = desc
  , pexp_loc        = loc
  , pexp_attributes = attrs
  }

data MkExpOpts = MkExpOpts
  { attrs  :: [Attribute]
  , loc    :: Location
  }

instance Default MkExpOpts where
  def = MkExpOpts
    { attrs  = []
    , loc    = default_loc
    }

caseExp :: Pattern -> Maybe Expression -> Expression -> Case
caseExp lhs guard rhs = Case
  { pc_lhs   = lhs
  , pc_guard = guard
  , pc_rhs   = rhs
  }

data MkTypeOpts = MkTypeOpts
  { attrs  :: [Attribute]
  , docs   :: ()
  , cstrs  :: [(Core_type, Core_type, Location)]
  , kind   :: Type_kind
  , loc    :: Location
  , params :: [(Core_type, Variance)]
  , priv   :: Private_flag
  , text   :: ()
  }

instance Default MkTypeOpts where
  def = MkTypeOpts
    { attrs  = []
    , cstrs  = []
    , docs   = () -- FIXME
    , kind   = Ptype_abstract
    , loc    = default_loc
    , params = []
    , priv   = Public
    , text   = () -- FIXME
    }

mkType :: MkTypeOpts -> Maybe Core_type -> Loc String -> Type_declaration
mkType (MkTypeOpts {..}) manifest name =
  Type_declaration
  { ptype_name       = name
  , ptype_params     = params
  , ptype_cstrs      = cstrs
  , ptype_kind       = kind
  , ptype_private    = priv
  , ptype_manifest   = manifest
  , ptype_attributes = attrs
  , ptype_loc        = loc
  }

mkOpn ::
  Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe Override_flag -> Loc Longident ->
  Open_description
mkOpn loc attrs docs override lid =
  Open_description
  { popen_lid        = lid
  , popen_override   = fromMaybe Fresh override
  , popen_loc        = fromMaybe default_loc loc
  , popen_attributes = add_docs_attrs (fromMaybe empty_docs docs) (fromMaybe [] attrs)
  }

mkstrexp :: Expression -> Attributes -> Structure_item
mkstrexp e attrs = Structure_item
  { pstr_desc = Pstr_eval e attrs
  , pstr_loc = pexp_loc e
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

text_str :: Int -> [Structure_item]
text_str pos = textStr (rhs_text pos)

attributeStr :: Maybe Location -> Attribute -> Structure_item
attributeStr mloc a = mkStr mloc (Pstr_attribute a)

textStr :: [Docstring] -> [Structure_item]
textStr text = map (\ ds -> attributeStr Nothing (text_attr ds)) text

mkmod :: Maybe [Attribute] -> Module_expr_desc -> Module_expr
mkmod attrs d = mkMod Nothing attrs d -- FIXME: Nothing

mkMod :: Maybe Location -> Maybe [Attribute] -> Module_expr_desc -> Module_expr
mkMod loc attrs d = Module_expr
  { pmod_desc       = d
  , pmod_loc        = fromMaybe default_loc loc
  , pmod_attributes = fromMaybe []          attrs
  }

mkMb ::
  Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe [Docstring] -> Loc String -> Module_expr ->
  Module_binding
mkMb loc attrs docs text name expr = Module_binding
  { pmb_name       = name
  , pmb_expr       = expr
  , pmb_attributes = add_text_attrs (fromMaybe [] text)
                     . add_docs_attrs (fromMaybe empty_docs docs)
                     $ fromMaybe [] attrs
  , pmb_loc        = fromMaybe default_loc loc
  }

mkPat :: MkPatOpts -> Pattern_desc -> Pattern
mkPat (MkPatOpts {..}) d = Pattern
  { ppat_desc       = d
  , ppat_loc        = loc
  , ppat_attributes = attrs
  }

data MkPatOpts = MkPatOpts
  { attrs :: [Attribute]
  , loc   :: Location
  }

instance Default MkPatOpts where
  def = MkPatOpts
    { attrs = []
    , loc   = default_loc
    }

mkpat :: Pattern_desc -> Pattern
mkpat d = mkPat def d -- FIXME

mkpatvar :: String -> Int -> Pattern
mkpatvar name pos = mkPat (def { loc = rhsLoc pos }) (Ppat_var (mkRHS name pos))

mkpat_cons :: Location -> Pattern -> Location -> Pattern
mkpat_cons consloc args loc =
  mkPat (def { loc }) (Ppat_construct (mkLoc (Lident "::") consloc) (Just args))

mklb :: t -> (Pattern, Expression) -> Attributes -> Let_binding
mklb _first (p, e) attrs = Let_binding
  { lb_pattern    = p
  , lb_expression = e
  , lb_attributes = attrs
  , lb_docs       = empty_docs -- symbol_docs_lazy ()
  , lb_text       = [] --if first then empty_text_lazy
                    --else symbol_text_lazy ()
  , lb_loc        = none -- symbol_rloc ()
  }

mklbs :: Maybe (Loc String) -> Rec_flag -> Let_binding -> Let_bindings
mklbs ext rf lb = Let_bindings
  { lbs_bindings  = [lb]
  , lbs_rec       = rf
  , lbs_extension = ext
  , lbs_loc       = none -- symbol_rloc ()
  }

addlb :: Let_bindings -> Let_binding -> Let_bindings
addlb lbs lb =
  lbs { lbs_bindings = lb : lbs_bindings lbs }

mkVb :: Maybe Location -> Maybe [(Loc String, Payload)] -> Maybe Docs ->
  Maybe Text -> Pattern -> Expression -> Value_binding
mkVb loc attrs docs text pat expr = Value_binding
  { pvb_pat        = pat
  , pvb_expr       = expr
  , pvb_attributes = add_text_attrs (fromMaybe [] text)
                     . add_docs_attrs (fromMaybe empty_docs docs)
                     $ fromMaybe [] attrs
  , pvb_loc        = fromMaybe default_loc loc
  }

val_of_let_bindings :: Let_bindings -> Structure_item
val_of_let_bindings lbs =
  let bindings =
        map
        (\ lb -> mkVb
                    (Just $ lb_loc lb)
                    (Just $ lb_attributes lb)
                    (Just $ lb_docs lb)
                    (Just $ lb_text lb)
                    (lb_pattern lb)
                    (lb_expression lb)
        )
        (lbs_bindings lbs)
  in
  let str = mkstr $ Pstr_value (lbs_rec lbs) (reverse bindings) in
  case lbs_extension lbs of
    Nothing -> str
    Just i  -> ghstr $ Pstr_extension (i, PStr [str]) []

expr_of_let_bindings :: Let_bindings -> Expression -> Expression
expr_of_let_bindings lbs body =
  let bindings =
        map
        (\ lb -> mkVb
                    (Just $ lb_loc lb)
                    (Just $ lb_attributes lb)
                    Nothing
                    Nothing
                    (lb_pattern lb)
                    (lb_expression lb)
        )
        (lbs_bindings lbs)
  in
  mkexp_attrs (Pexp_let (lbs_rec lbs) (reverse bindings) body) (lbs_extension lbs, [])

wrap_exp_attrs :: Expression -> (Maybe (Loc String), [Attribute]) -> Expression
wrap_exp_attrs body (ext, attrs) =
  let body1 = body { pexp_attributes = attrs ++ pexp_attributes body } in
  case ext of
    Nothing -> body1
    Just i  -> ghexp $ Pexp_extension (i, (PStr [mkstrexp body1 []]))

mkexp_attrs :: Expression_desc -> (Maybe (Loc String), [Attribute]) -> Expression
mkexp_attrs d attrs = wrap_exp_attrs (mkexp d) attrs

mkexp :: Expression_desc -> Expression
mkexp d = mkExp def d -- FIXME

mktyp :: Core_type_desc -> Core_type
mktyp d = mkTyp def d -- FIXME

ghexp :: Expression_desc -> Expression
ghexp d = mkExp def d -- FIXME

ghpat :: Pattern_desc -> Pattern
ghpat d = mkPat def d -- FIXME

ghtyp :: Core_type_desc -> Core_type
ghtyp d = mkTyp def d -- FIXME

mkexp_constraint :: Expression -> (Maybe Core_type, Maybe Core_type) -> Expression
mkexp_constraint e (t1, t2) = case (t1, t2) of
  (Just t,  Nothing) -> ghexp $ Pexp_constraint e t
  (_,       Just t)  -> ghexp $ Pexp_coerce e t1 t
  (Nothing, Nothing) -> error "This should not happen"

reloc_exp :: Expression -> Expression
reloc_exp e = e { pexp_loc = none } -- FIXME

extra_rhs_core_type :: Core_type -> a -> Core_type
extra_rhs_core_type ct _pos =
  -- FIXME
  -- let docs = rhs_info pos in
  ct -- { ptyp_attributes = add_info_attrs docs (ptyp_attributes ct) }

mkTe :: MkTeOpts -> Loc Longident -> [ExtensionConstructor] -> TypeExtension
mkTe (MkTeOpts {..}) path constructors = TypeExtension
  { ptyext_path         = path
  , ptyext_params       = params
  , ptyext_constructors = constructors
  , ptyext_private      = priv
  , ptyext_attributes   = attrs
  }

data MkTeOpts = MkTeOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  , params :: [(Core_type, Variance)]
  , priv   :: Private_flag
  }

mk_exception :: MkExceptionOpts -> ExtensionConstructor -> TypeException
mk_exception (MkExceptionOpts {..}) ctor = TypeException
  { ptyexn_constructor = ctor
  , ptyexn_attributes  = attrs
  }

data MkExceptionOpts = MkExceptionOpts
  { attrs  :: [Attribute]
  , docs   :: Docs
  }

instance Default MkExceptionOpts where
  def = MkExceptionOpts
    { attrs  = []
    , docs   = empty_docs
    }

rebind :: RebindOpts -> Loc String -> Loc Longident -> ExtensionConstructor
rebind (RebindOpts {..}) name lid = ExtensionConstructor
  { pext_name       = name
  , pext_kind       = Pext_rebind lid
  , pext_loc        = loc
  , pext_attributes = add_docs_attrs docs (add_info_attrs info attrs)
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
    , docs   = empty_docs
    , loc    = default_loc
    , info   = empty_info
    }

decl :: DeclOpts -> Maybe Core_type -> Loc String -> ExtensionConstructor
decl (DeclOpts {..}) res name = ExtensionConstructor
  { pext_name       = name
  , pext_kind       = Pext_decl args res
  , pext_loc        = loc
  , pext_attributes = add_docs_attrs docs (add_info_attrs info attrs)
  }

data DeclOpts = DeclOpts
  { args   :: Constructor_arguments
  , attrs  :: [Attribute]
  , docs   :: Docs
  , loc    :: Location
  , info   :: Info
  }

instance Default DeclOpts where
  def = DeclOpts
    { args   = Pcstr_tuple []
    , attrs  = []
    , docs   = empty_docs
    , loc    = default_loc
    , info   = empty_info
    }

symbol_rloc :: () -> Location
symbol_rloc () = none -- FIXME

reloc_pat :: Pattern -> Pattern
reloc_pat e = e { ppat_loc = symbol_rloc () }

mkinfix :: Expression -> String -> Expression -> Expression
mkinfix arg1 name arg2 =
  mkexp $ Pexp_apply (mkoperator name 2) [(Nolabel, arg1), (Nolabel, arg2)]

mkoperator :: String -> Int -> Expression
mkoperator name pos =
  let loc = rhsLoc pos in
  mkExp (def { loc }) $ Pexp_ident (mkLoc (Lident name) loc)

constructor ::
  ConstructorOpts ->
  Maybe Core_type ->
  Loc String ->
  Constructor_declaration
constructor (ConstructorOpts {..}) res name =
  Constructor_declaration
  { pcd_name       = name
  , pcd_args       = args
  , pcd_res        = res
  , pcd_loc        = loc
  , pcd_attributes = attrs
  }

data ConstructorOpts = ConstructorOpts
  { args   :: Constructor_arguments
  , attrs  :: [Attribute]
  , loc    :: Location
  , info   :: Info
  }

instance Default ConstructorOpts where
  def = ConstructorOpts
    { args = Pcstr_tuple []
    , attrs = []
    , loc   = default_loc
    , info  = empty_info
    }

mktailexp :: Location -> [Expression] -> Expression
mktailexp nilloc = \case
  [] ->
    let loc = nilloc { loc_ghost = True } in
    let nil = Loc { txt = Lident "[]", loc } in
    mkExp (def { loc }) $ Pexp_construct nil Nothing
  e1 : el ->
    let exp_el = mktailexp nilloc el in
    let loc = Location
          { loc_start = loc_start . pexp_loc $ e1
          , loc_end   = loc_end   . pexp_loc $ e1
          , loc_ghost = True
          }
    in
    let arg = mkExp (def { loc }) $ Pexp_tuple [e1, exp_el] in
    mkexp_cons (loc {loc_ghost = True}) arg loc

mkexp_cons :: Location -> Expression -> Location -> Expression
mkexp_cons consloc args loc = mkExp (def { loc }) $ Pexp_construct (mkLoc (Lident "::") consloc) (Just args)

mkexp_opt_constraint :: Expression -> Maybe (Maybe Core_type, Maybe Core_type) -> Expression
mkexp_opt_constraint e = \case
  Nothing -> e
  Just constraint -> mkexp_constraint e constraint

mkpat_opt_constraint :: Pattern -> Maybe Core_type -> Pattern
mkpat_opt_constraint p = \case
  Nothing -> p
  Just typ -> mkpat (Ppat_constraint p typ)

pat_of_label :: Longident -> Int -> Pattern
pat_of_label lbl pos = mkpat $ Ppat_var $ mkRHS (Longident.last lbl) pos

mktailpat :: Location -> [Pattern] -> Pattern
mktailpat nilloc = \case
  [] ->
    let loc = nilloc { loc_ghost = True } in
    let nil = Loc { txt = Lident "[]", loc } in
    mkPat (def { loc }) $ Ppat_construct nil Nothing
  p1 : pl ->
    let pat_pl = mktailpat nilloc pl in
    let loc = Location
          { loc_start = loc_start $ ppat_loc p1
          , loc_end   = loc_end   $ ppat_loc pat_pl
          , loc_ghost = True
          }
    in
    let arg = mkPat (def { loc }) $ Ppat_tuple [p1, pat_pl] in
    mkpat_cons (loc { loc_ghost = True }) arg loc
