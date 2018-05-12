module Language.OCaml.Parser.Common
  ( addlb
  , caseExp
  , expr_of_let_bindings
  , ghexp
  , ghpat
  , ghtyp
  , ident_P
  , mkExp
  , mkexp
  , mkexp_attrs
  , mkexp_constraint
  , mkLoc
  , mklb
  , mklbs
  , mkMb
  , mkMod
  , mkmod
  , mkOpn
  , mkPat
  , mkpat
  , mkpatvar
  , mkRHS
  , mkstr_ext
  , mkstrexp
  , mkTyp
  , mkType
  , text_str
  , val_of_let_bindings
  ) where

import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Language.OCaml.Definitions.Parsing.ASTTypes as ASTTypes
import           Language.OCaml.Definitions.Parsing.Docstrings
import           Language.OCaml.Definitions.Parsing.Location
import           Language.OCaml.Definitions.Parsing.ParseTree
import           Language.OCaml.Definitions.Parsing.Parser.LetBinding
import           Language.OCaml.Definitions.Parsing.Parser.LetBindings
import           Language.OCaml.Parser.Tokens
import           Language.OCaml.Parser.Utils.Utils

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

wrap_str_ext :: Structure_item -> Maybe (ASTTypes.Loc String) -> Structure_item
wrap_str_ext body ext = case ext of
  Nothing -> body
  Just i -> ghstr $ Pstr_extension (i, PStr [body]) []

mkstr_ext :: Structure_item_desc -> Maybe (ASTTypes.Loc String) -> Structure_item
mkstr_ext d ext = wrap_str_ext (mkstr d) ext

mkTyp :: Core_type_desc -> Core_type
mkTyp p = Core_type { ptyp_desc = p }

mkExp :: Maybe Location -> Maybe Attributes -> Expression_desc -> Expression
mkExp loc attrs desc =
  Expression
  { pexp_desc       = desc
  , pexp_loc        = fromMaybe default_loc loc
  , pexp_attributes = fromMaybe []          attrs
  }

caseExp :: Pattern -> Maybe Expression -> Expression -> Case
caseExp lhs guard rhs = Case
  { pc_lhs   = lhs
  , pc_guard = guard
  , pc_rhs   = rhs
  }

mkType ::
  [(Core_type, Variance)] ->
  [(Core_type, Core_type, Location)] ->
  Type_kind ->
  Private_flag ->
  Maybe Core_type ->
  ASTTypes.Loc String ->
  Type_declaration
mkType {- loc attrs docs text -} params cstrs kind priv manifest name =
  Type_declaration
  { ptype_name     = name
  , ptype_params   = params
  , ptype_cstrs    = cstrs
  , ptype_kind     = kind
  , ptype_private  = priv
  , ptype_manifest = manifest
  --, ptype_attributes :: attributes
  --, ptype_loc :: Location.t
  }

mkOpn ::
  Maybe Location -> Maybe [(ASTTypes.Loc String, Payload)] -> Maybe Docs ->
  Maybe ASTTypes.Override_flag -> ASTTypes.Loc Longident ->
  Open_description
mkOpn loc attrs docs override lid =
  Open_description
  { popen_lid        = lid
  , popen_override   = fromMaybe ASTTypes.Fresh override
  , popen_loc        = fromMaybe default_loc loc
  , popen_attributes = add_docs_attrs (fromMaybe empty_docs docs) (fromMaybe [] attrs)
  }

mkstrexp :: Expression -> Attributes -> Structure_item
mkstrexp e attrs = Structure_item
  { pstr_desc = Pstr_eval e attrs
  , pstr_loc = pexp_loc e
  }

mkLoc :: a -> Location -> ASTTypes.Loc a
mkLoc t l = ASTTypes.Loc
  { ASTTypes.txt = t
  , ASTTypes.loc = l
  }

rhsLoc :: t -> Location
rhsLoc _ = none -- FIXME

mkRHS :: a -> t -> ASTTypes.Loc a
mkRHS rhs pos = mkLoc rhs (rhsLoc pos)

text_str :: a -> [Structure_item]
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
  Maybe Location -> Maybe [(ASTTypes.Loc String, Payload)] -> Maybe Docs ->
  Maybe [Docstring] -> ASTTypes.Loc String -> Module_expr ->
  Module_binding
mkMb loc attrs docs text name expr = Module_binding
  { pmb_name       = name
  , pmb_expr       = expr
  , pmb_attributes = add_text_attrs (fromMaybe [] text)
                     . add_docs_attrs (fromMaybe empty_docs docs)
                     $ fromMaybe [] attrs
  , pmb_loc        = fromMaybe default_loc loc
  }

mkPat ::
  Maybe Location -> Maybe [(ASTTypes.Loc String, Payload)] -> Pattern_desc ->
  Pattern
mkPat loc attrs d = Pattern
  { ppat_desc       = d
  , ppat_loc        = fromMaybe default_loc loc
  , ppat_attributes = fromMaybe [] attrs
  }

mkpat :: Pattern_desc -> Pattern
mkpat d = mkPat Nothing Nothing d -- FIXME

mkpatvar :: String -> t -> Pattern
mkpatvar name pos = mkPat (Just (rhsLoc pos)) Nothing (Ppat_var (mkRHS name pos))

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

mklbs :: Maybe (ASTTypes.Loc String) -> ASTTypes.Rec_flag -> Let_binding -> Let_bindings
mklbs ext rf lb = Let_bindings
  { lbs_bindings  = [lb]
  , lbs_rec       = rf
  , lbs_extension = ext
  , lbs_loc       = none -- symbol_rloc ()
  }

addlb :: Let_bindings -> Let_binding -> Let_bindings
addlb lbs lb =
  lbs { lbs_bindings = lb : lbs_bindings lbs }

mkVb :: Maybe Location -> Maybe [(ASTTypes.Loc String, Payload)] -> Maybe Docs ->
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

wrap_exp_attrs :: Expression -> (Maybe (ASTTypes.Loc String), [Attribute]) -> Expression
wrap_exp_attrs body (ext, attrs) =
  let body1 = body { pexp_attributes = attrs ++ pexp_attributes body } in
  case ext of
    Nothing -> body1
    Just i  -> ghexp $ Pexp_extension (i, (PStr [mkstrexp body1 []]))

mkexp_attrs :: Expression_desc -> (Maybe (ASTTypes.Loc String), [Attribute]) -> Expression
mkexp_attrs d attrs = wrap_exp_attrs (mkexp d) attrs

mkexp :: Expression_desc -> Expression
mkexp d = mkExp Nothing Nothing d -- FIXME

ghexp :: Expression_desc -> Expression
ghexp d = mkExp Nothing Nothing d -- FIXME

ghpat :: Pattern_desc -> Pattern
ghpat d = mkPat Nothing Nothing d -- FIXME

ghtyp :: Core_type_desc -> Core_type
ghtyp d = mkTyp d -- FIXME

mkexp_constraint :: Expression -> (Maybe Core_type, Maybe Core_type) -> Expression
mkexp_constraint e (t1, t2) = case (t1, t2) of
  (Just t,  Nothing) -> ghexp $ Pexp_constraint e t
  (_,       Just t)  -> ghexp $ Pexp_coerce e t1 t
  (Nothing, Nothing) -> error "This should not happen"
