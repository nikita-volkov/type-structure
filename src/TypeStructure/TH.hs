module TypeStructure.TH where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.Prelude.TH
import qualified TypeStructure.Class as Class
import qualified TypeStructure.TH.Model as M
import qualified TypeStructure.TH.Template as Template


-- |
-- Automatically derive the instance of 'Class.TypeStructure' using Template Haskell.
derive :: Name -> Q [Dec]
derive name = do
  typeCon <- deriveTypeCon name
  info <- reify name
  let
    declaration = infoToDeclaration info
    vars = case declaration of
      M.Primitive arity -> arity |> take |> ($ [0..]) |> map (show >>> ('_':))
      M.ADT vars _ -> vars
      M.Synonym vars _ -> vars
  (typesWithoutDictionaries, typesWithDictionaries) <- 
    partitionEithers <$> 
    execStateT (analyzeReferredTypes name) []
  inlinedRecords <- forM typesWithoutDictionaries $ \n -> do
    typeCon <- deriveTypeCon n
    declaration <- infoToDeclaration <$> reify n
    return (typeCon, declaration)
  return $ (:[]) $ Template.renderInstance 
    name vars typeCon (nub $ (typeCon, declaration) : inlinedRecords) typesWithDictionaries
  where
    deriveTypeCon name = do
      ns <- maybe (fail "Name without namespace") return $ nameModule name
      return (ns, nameBase name)

analyzeReferredTypes :: 
  Name -> 
  StateT [Either Name Type] Q ()
analyzeReferredTypes name = do
  info <- lift $ reify $ name
  forM_ (referredTypes info) analyzeType
  where
    analyzeType t = do
      (lift $ isProperInstance' ''Class.TypeStructure [t]) >>= \case
        True -> void $ insert $ Right $ t
        False -> case t of
          AppT l r -> analyzeType l >> analyzeType r
          ConT n -> do
            inserted <- insert $ Left $ n
            when inserted $ analyzeReferredTypes n
          _ -> return ()
    insert a = state $ \list ->
      if elem a list
        then (False, list)
        else (True, a : list)

referredTypes :: Info -> [Type]
referredTypes = \case
  TyConI d -> case d of
    DataD _ _ _ cons _ -> conTypes =<< cons
    NewtypeD _ _ _ con _ -> conTypes $ con
    TySynD _ _ t -> [t]
    d -> $bug $ "Unsupported dec: " <> show d
  PrimTyConI n arity _ -> []
  i -> $bug $ "Unsupported info: " <> show i

conTypes :: Con -> [Type]
conTypes = \case
  NormalC n ts -> map snd ts
  RecC n ts -> map (\(_, _, t) -> t) ts
  InfixC (_, l) n (_, r) -> [l, r]
  c -> $bug $ "Unexpected constructor: " <> show c

adaptType :: Type -> M.Type
adaptType = \case
  AppT l r -> M.App (adaptType l) (adaptType r)
  VarT n -> M.Var $ nameBase $ n
  ConT n -> fromName n
  TupleT a -> fromName $ tupleTypeName a
  UnboxedTupleT a -> fromName $ unboxedTupleTypeName a
  ArrowT -> fromName ''(->)
  ListT -> fromName ''[]
  t -> $bug $ "Unsupported type: " <> show t
  where
    fromName n = 
      M.Con $ 
        adaptTypeConName n ?: 
        ($bug $ "Name has no namespace: " <> show n)

adaptTypeConName :: Name -> Maybe M.TypeCon
adaptTypeConName n = do
  ns <- nameModule n
  return (ns, nameBase n)

infoToDeclaration :: Info -> M.Declaration
infoToDeclaration = \case
  TyConI d -> case d of
    DataD _ _ vars cons _ -> M.ADT (map adaptVar vars) (map adaptCon cons)
    NewtypeD _ _ vars con _ -> M.ADT (map adaptVar vars) [adaptCon con]
    TySynD _ vars t -> M.Synonym (map adaptVar vars) (adaptType t)
    d -> $bug $ "Unsupported dec: " <> show d
  PrimTyConI _ arity _ -> M.Primitive arity
  i -> $bug $ "Unsupported info: " <> show i

adaptVar :: TyVarBndr -> M.TypeVar
adaptVar = \case
  PlainTV n -> nameBase n
  KindedTV n k -> nameBase n

adaptCon :: Con -> M.Constructor
adaptCon = \case
  NormalC n ts -> (nameBase n, map (\(_, t) -> adaptType t) ts)
  RecC n ts -> (nameBase n, map (\(_, _, t) -> adaptType t) ts)
  InfixC (_, a) n (_, b) -> (nameBase n, [adaptType a, adaptType b])
  c -> $bug $ "Unexpected constructor: " <> show c
