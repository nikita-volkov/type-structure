module TypeStructure.TH where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.Prelude.TH
import qualified TypeStructure.Class as Class
import qualified TypeStructure.TH.Analysis as A
import qualified TypeStructure.TH.Template as Template


derive :: Name -> Q [Dec]
derive name = do
  typeCon <- deriveTypeCon name
  info <- reify name
  let
    declaration = A.infoToDeclaration info
    vars = case declaration of
      A.Primitive -> []
      A.ADT vars _ -> vars
  (typesWithoutDictionaries, typesWithDictionaries) <- 
    partitionEithers <$> 
    execStateT (analyzeReferredTypes name) []
  inlinedRecords <- forM typesWithoutDictionaries $ \n -> do
    typeCon <- deriveTypeCon n
    declaration <- A.infoToDeclaration <$> reify n
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
      (lift $ isInstance' ''Class.TypeStructure [t]) >>= \case
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
    d -> $bug $ "Unsupported dec: " <> show d
  PrimTyConI n arity _ -> []
  i -> $bug $ "Unsupported info: " <> show i

conTypes :: Con -> [Type]
conTypes = \case
  NormalC n ts -> map snd ts
  RecC n ts -> map (\(_, _, t) -> t) ts
  InfixC (_, l) n (_, r) -> [l, r]
  c -> $bug $ "Unexpected constructor: " <> show c

