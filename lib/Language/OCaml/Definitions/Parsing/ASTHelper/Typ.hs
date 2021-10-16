{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

module Language.OCaml.Definitions.Parsing.ASTHelper.Typ
  ( MkOpts (..),
    mk,
    varifyConstructors,
  )
where

import Control.Eff (Eff, Member)
import Control.Eff.Exception (Exc, throwError)
import Control.Monad (when)
import Data.Default (Default (..))
import Language.OCaml.Definitions.Parsing.ASTHelper.Common
  ( defaultLoc,
  )
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( Loc (Loc, txt),
  )
import Language.OCaml.Definitions.Parsing.Location (Location)
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Attribute,
    CoreType (..),
    CoreTypeDesc (..),
    Longident (Lident),
    ObjectField (..),
    RowField (..),
  )
import Language.OCaml.Utils ((<*^>))

mk :: MkOpts -> CoreTypeDesc -> CoreType
mk (MkOpts {..}) desc =
  CoreType
    { ptypDesc = desc,
      ptypLoc = loc,
      ptypAttributes = attrs
    }

data MkOpts = MkOpts
  { loc :: Location,
    attrs :: [Attribute]
  }

instance Default MkOpts where
  def =
    MkOpts
      { loc = defaultLoc,
        attrs = []
      }

varifyConstructors ::
  (Member (Exc String) r) =>
  [Loc String] ->
  CoreType ->
  Eff r CoreType
varifyConstructors varNames0 = loop
  where
    checkVariable :: (Member (Exc String) r) => [String] -> Location -> String -> Eff r ()
    checkVariable vl pos v =
      when (v `elem` vl) $
        throwError $ "Variable in scope " ++ show v ++ " at " ++ show pos

    loopRowField :: (Member (Exc String) r) => RowField -> Eff r RowField
    loopRowField = \case
      Rtag label attrs flag lst -> Rtag label attrs flag <$> traverse loop lst
      Rinherit t -> Rinherit <$> loop t

    loopObjectField :: (Member (Exc String) r) => ObjectField -> Eff r ObjectField
    loopObjectField = \case
      Otag label attrs t -> Otag label attrs <$> loop t
      Oinherit t -> Oinherit <$> loop t

    varNames = map txt varNames0

    loop :: (Member (Exc String) r) => CoreType -> Eff r CoreType
    loop t = do
      desc <- case ptypDesc t of
        PtypAny -> return PtypAny
        PtypVar x -> do
          checkVariable varNames (ptypLoc t) x
          return $ PtypVar x
        PtypArrow label coreType coreType' -> PtypArrow label <$> loop coreType <*> loop coreType'
        PtypTuple lst -> PtypTuple <$> traverse loop lst
        PtypConstr (Loc {txt = Lident s}) [] | s `elem` varNames -> return $ PtypVar s
        PtypConstr longident lst -> PtypConstr longident <$> traverse loop lst
        PtypObject lst o -> PtypObject <$> traverse loopObjectField lst <*^> o
        PtypClass longident lst -> PtypClass longident <$> traverse loop lst
        PtypAlias coreType string -> do
          checkVariable varNames (ptypLoc t) string
          PtypAlias <$> loop coreType <*^> string
        PtypVariant rowFieldList flag lblLstOption ->
          PtypVariant <$> traverse loopRowField rowFieldList <*^> flag <*^> lblLstOption
        PtypPoly _stringLst _coreType -> error "TODO"
        PtypPackage (_longident, _lst) -> error "TODO"
        PtypExtension (s, arg) -> return $ PtypExtension (s, arg)
      return $ t {ptypDesc = desc}
