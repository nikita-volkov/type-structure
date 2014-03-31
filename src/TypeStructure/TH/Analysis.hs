module TypeStructure.TH.Analysis where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import qualified Language.Haskell.TH as T


-- NOTE!
-- Oddly enough, this model completely replicates the output Model itself, so... 
-- Refactoring, baby!

data Type =
  App Type Type |
  Var TypeVar |
  Con TypeCon
  deriving (Show, Eq, Ord)

type TypeCon = (Namespace, Name)

type Namespace = String

type Name = String

data Declaration = 
  Primitive |
  ADT [TypeVar] [Constructor]
  deriving (Show, Eq, Ord)

type TypeVar = Name

type Constructor = (Name, [Type])


adaptType :: T.Type -> Type
adaptType = \case
  T.AppT l r -> App (adaptType l) (adaptType r)
  T.VarT n -> Var $ T.nameBase $ n
  T.ConT n -> fromName n
  T.TupleT a -> fromName $ T.tupleTypeName a
  T.UnboxedTupleT a -> fromName $ T.unboxedTupleTypeName a
  T.ArrowT -> fromName ''(->)
  T.ListT -> fromName ''[]
  t -> $bug $ "Unsupported type: " <> show t
  where
    fromName n = 
      Con $ 
        adaptTypeConName n ?: 
        ($bug $ "Name has no namespace: " <> show n)

adaptTypeConName :: T.Name -> Maybe TypeCon
adaptTypeConName n = do
  ns <- T.nameModule n
  return (ns, T.nameBase n)

referredTypes :: T.Info -> [T.Type]
referredTypes = \case
  T.TyConI d -> case d of
    T.DataD _ _ _ cons _ -> conTypes =<< cons
    T.NewtypeD _ _ _ con _ -> conTypes $ con
    d -> $bug $ "Unsupported dec: " <> show d
  T.PrimTyConI n arity _ -> []
  i -> $bug $ "Unsupported info: " <> show i
  where
    conTypes :: T.Con -> [T.Type]
    conTypes = \case
      T.NormalC n ts -> map snd ts
      T.RecC n ts -> map (\(_, _, t) -> t) ts
      T.InfixC (_, l) n (_, r) -> [l, r]
      c -> $bug $ "Unexpected constructor: " <> show c

infoToDeclaration :: T.Info -> Declaration
infoToDeclaration = \case
  T.TyConI d -> case d of
    T.DataD _ _ vars cons _ -> adt vars cons
    T.NewtypeD _ _ vars con _ -> adt vars [con]
    d -> $bug $ "Unsupported dec: " <> show d
  T.PrimTyConI _ arity _ -> Primitive
  i -> $bug $ "Unsupported info: " <> show i
  where
    adt vars cons = ADT vars' cons' where
      vars' = flip map vars $ \case
        T.PlainTV n -> T.nameBase n
        v -> $bug $ "Unexpected type of var: " <> show v
      cons' = flip map cons $ \case
        T.NormalC n ts -> (T.nameBase n, map (\(_, t) -> adaptType t) ts)
        T.RecC n ts -> (T.nameBase n, map (\(_, _, t) -> adaptType t) ts)
        T.InfixC (_, a) n (_, b) -> (T.nameBase n, [adaptType a, adaptType b])
        c -> $bug $ "Unexpected constructor: " <> show c
