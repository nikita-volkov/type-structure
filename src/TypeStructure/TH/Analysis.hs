module TypeStructure.TH.Analysis where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import Language.Haskell.TH
import Control.Lens
import qualified TypeStructure.Graph as G
import qualified TypeStructure.TH.Templates.Instance as InstanceTpl
import qualified TypeStructure.TH.Templates.Declaration as DeclarationTpl
import qualified TypeStructure.TH.Templates.Type as TypeTpl
import qualified TypeStructure.TH.Templates.TypeCon as TypeConTpl


type Result = (DeclarationTpl.Settings, ReferredTypeCons)

type ReferredTypeCons = [Name]

infoAnalysis :: Info -> Result
infoAnalysis = \case
  TyConI d -> case d of
    DataD _ _ vars cons _ -> let 
      dtvl = map tyVarBndrAnalysis vars
      (dtcl, rtcl) = unzip $ map conAnalysis $ cons
      dts = DeclarationTpl.ADT dtvl dtcl
      in (dts, asum rtcl)
    NewtypeD _ _ vars con _ -> let 
      dtvl = map tyVarBndrAnalysis vars
      (dtc, rtc) = conAnalysis $ con
      dts = DeclarationTpl.ADT dtvl [dtc]
      in (dts, rtc)
    d -> $bug $ "Unexpected type of declaration: " <> show d
  PrimTyConI n arity _ -> (DeclarationTpl.Primitive, [n])
  i -> $bug $ "Unexpected type of info: " <> show i

tyVarBndrAnalysis :: TyVarBndr -> (DeclarationTpl.Var)
tyVarBndrAnalysis = \case
  PlainTV n -> nameBase n
  v -> $bug $ "Unexpected type of var: " <> show v

conAnalysis :: Con -> (DeclarationTpl.Constructor, ReferredTypeCons)
conAnalysis = \case
  NormalC n ts -> let
    (ttsl, rtcl) = unzip $ map (\(_, t) -> typeAnalysis t) $ ts
    in ((nameBase n, ttsl), asum rtcl)
  RecC n ts -> let
    (ttsl, rtcl) = unzip $ map (\(_, _, t) -> typeAnalysis t) $ ts
    in ((nameBase n, ttsl), asum rtcl)
  InfixC (_, l) n (_, r) -> let
    (ltts, lrtc) = typeAnalysis l
    (rtts, rrtc) = typeAnalysis r
    in ((nameBase n, [ltts, rtts]), lrtc <|> rrtc)
  c -> $bug $ "Unexpected constructor: " <> show c

typeAnalysis :: Type -> (TypeTpl.Settings, ReferredTypeCons)
typeAnalysis = \case
  AppT l r -> let 
    (ltts, lrtc) = typeAnalysis l
    (rtts, rrtc) = typeAnalysis r
    in (TypeTpl.App ltts rtts, lrtc <|> rrtc)
  VarT n -> (TypeTpl.Var $ nameBase $ n, empty)
  ConT n -> (typeConFromName n, pure n)
  TupleT a -> (typeConFromName $ tupleTypeName a, empty)
  UnboxedTupleT a -> (typeConFromName $ unboxedTupleTypeName a, empty)
  ArrowT -> (typeConFromName ''(->), empty)
  ListT -> (typeConFromName ''[], empty)
  t -> $bug $ "Unsupported type: " <> show t
  where
    typeConFromName n = TypeTpl.Con (ns, nameBase n) where
      ns = nameModule n ?: ($bug $ "Name has no namespace: " <> show n)

