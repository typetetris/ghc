-- Exists to allow TyCoRep to import pretty-printers
module IfaceType where

import Var (TyVarBinder, TyVarBndr(..), TyVar, ArgFlag)
import {-# SOURCE #-} TyCoRep (Type, TyLit, TyPrec, Coercion)
import TyCon (TyCon, TyConBndrVis)
import Outputable (Outputable, SDoc)
import FastString (FastString)

type IfLclName = FastString
type IfaceKind = IfaceType

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceTcArgs
type IfaceTvBndr = (IfLclName, IfaceKind)
type IfaceTyConBinder = TyVarBndr IfaceTvBndr TyConBndrVis
type IfaceForAllBndr  = TyVarBndr IfaceTvBndr ArgFlag

instance Outputable IfaceType

pprIfaceType, pprParendIfaceType :: IfaceType -> SDoc
pprIfaceSigmaType :: IfaceType -> SDoc
pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceTvBndr :: Bool -> IfaceTvBndr -> SDoc
pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceContext :: Outputable a => [a] -> SDoc
pprIfaceContextArr :: Outputable a => [a] -> SDoc
pprIfaceTypeApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfaceCoTcApp :: TyPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprTyTcApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfacePrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc

toIfaceType :: Type -> IfaceType
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceCoercion :: Coercion -> IfaceCoercion
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
