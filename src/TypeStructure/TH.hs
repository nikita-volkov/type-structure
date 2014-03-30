module TypeStructure.TH where

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

derive :: Name -> Q [Dec]
derive name = do
  (typeVars, typeConM, inlinedDeclarations, inheritedDictionaries)
    <- execStateT accumulateInstanceSettings (mempty, Nothing, mempty, mempty)
  let 
    typeCon = typeConM ?: $bug "TypeCon not set"
    settings = (name, typeVars, typeCon, inlinedDeclarations, inheritedDictionaries)
    in return $ (:[]) $ InstanceTpl.render settings
  where
    accumulateInstanceSettings = do
      zoom _2 $ do
        ns <- maybe (fail "Name without namespace") return $ nameModule name
        id ?= (ns, nameBase name)
      (lift $ reify name) >>= \case
        TyConI d -> case d of
          DataD _ _ vars cons _ -> do
            mapM_ visitVar vars
          NewtypeD _ _ vars con _ -> do
            mapM_ visitVar vars
          d -> fail $ "Unsupported declaration: " <> show d
        PrimTyConI _ arity _ -> do
          let 
            names = [1..] & take arity & map (('_':) . show)
            in mapM_ visitVarName names
          $notImplemented
        _ -> $notImplemented
      -- traverseConType name
      -- set _4 collectDictionaryRecords
      where
        visitVar = \case
          PlainTV n -> do
            visitVarName $ nameBase $ n
          v -> fail $ "Var of unsupported type: " <> show v
        visitVarName n = do
          _1 %= (n :)
        -- -- |
        -- -- Traverses the type constructor names deeply up until it reaches the ones,
        -- -- which either have instances or are primitive types.
        -- traverseConType name = do
        --   zoom _1 $ do
        --     ns <- maybe (fail "Name without namespace") return $ nameModule name
        --     id ?= (ns, nameBase name)

        --   (lift $ reify name) >>= \case
        --     TyConI d -> case d of
        --       DataD _ _ vars cons _ -> do
        --         mapM_ visitVar vars
        --         mapM_ visitCon cons
        --       NewtypeD _ _ vars con _ -> do
        --         mapM_ visitVar vars
        --         visitCon con
        --       d -> fail $ "Unsupported declaration: " <> show d
        --     PrimTyConI _ arity _ -> do
        --       _2 ?= DeclarationTpl.Primitive
        --     _ -> $notImplemented
        --   where
        --     visitVar = \case
        --       PlainTV n -> $notImplemented
        --       v -> fail $ "Var of unsupported type: " <> show v
        --     visitCon = \case
        --       NormalC n ts -> do
        --         forM ts $ \(_, t) -> $notImplemented
        --         $notImplemented

-- -- traverseReferredTypesWithNoInstance :: Name -> (Info -> m r) -> m [r]
-- traverseReferredTypesWithNoInstance n f = do
--   (lift $ reify n) >>= \case
--     TyConI d -> case d of

infoToDeclarationTplSettings :: Info -> Maybe DeclarationTpl.Settings
infoToDeclarationTplSettings = \case
  TyConI d -> case d of
    DataD _ _ vars cons _ -> Just $ adt vars cons
    NewtypeD _ _ vars con _ -> Just $ adt vars [con]
    _ -> Nothing
  PrimTyConI _ arity _ -> Just $ DeclarationTpl.Primitive
  _ -> Nothing 
  where
    adt vars cons = DeclarationTpl.ADT vars' cons' where
      vars' = flip map vars $ \case
        PlainTV n -> nameBase n
        v -> $bug $ "Unexpected type of var: " <> show v
      cons' = flip map cons $ \case
        NormalC n ts -> (nameBase n, map (\(_, t) -> typeToTypeTplSettings t) ts)
        RecC n ts -> (nameBase n, map (\(_, _, t) -> typeToTypeTplSettings t) ts)
        InfixC (_, a) n (_, b) -> (nameBase n, [typeToTypeTplSettings a, typeToTypeTplSettings b])
        c -> $bug $ "Unexpected constructor: " <> show c

typeToTypeTplSettings :: Type -> TypeTpl.Settings
typeToTypeTplSettings = \case
  AppT l r -> TypeTpl.App (typeToTypeTplSettings l) (typeToTypeTplSettings r)
  VarT n -> TypeTpl.Var $ nameBase $ n
  ConT n -> fromName n
  TupleT a -> fromName $ tupleTypeName a
  UnboxedTupleT a -> fromName $ unboxedTupleTypeName a
  ArrowT -> fromName ''(->)
  ListT -> fromName ''[]
  t -> $bug $ "Unsupported type: " <> show t
  where
    fromName n = TypeTpl.Con (ns, nameBase n) where
      ns = nameModule n ?: ($bug $ "Name has no namespace: " <> show n)

conTypes :: Con -> [Type]
conTypes = \case
  NormalC n ts -> map snd ts
  RecC n ts -> map (\(_, _, t) -> t) ts
  InfixC (_, a) n (_, b) -> [a, b]
  c -> $bug $ "Unexpected constructor: " <> show c



-- analyzeTypeConName :: Name -> StateT ([String], (String, String), )
-- analyzeTypeConName = do
--   zoom _2 $ do
--     ns <- maybe (fail "Name without namespace") return $ nameModule name
--     id ?= (ns, nameBase name)
--   (lift $ reify name) >>= \case
--     TyConI d -> case d of
--       DataD _ _ vars cons _ -> do
--         mapM_ visitVar vars
--         mapM_ visitCon cons
--       NewtypeD _ _ vars con _ -> do
--         mapM_ visitVar vars
--         visitCon con
--       d -> fail $ "Unsupported declaration: " <> show d
--     PrimTyConI _ arity _ -> do
--       let 
--         names = [1..] & take arity & map (('_':) . show)
--         in mapM_ visitVarName names
--       $notImplemented
--     _ -> $notImplemented
--   where
--     visitVar = \case
--       PlainTV n -> do
--         visitVarName $ nameBase $ n
--       v -> fail $ "Var of unsupported type: " <> show v
--     visitVarName n = do
--       _1 %= (n :)
--     visitCon = \case
--       NormalC n ts -> do
--         forM ts $ \(_, t) -> $notImplemented
--         $notImplemented
--       -- RecC n ts -> map (\(_, _, t) -> t) ts
--       -- InfixC (_, a) n (_, b) -> [a, b]
--       -- c -> $bug $ "Unexpected constructor: " <> show c

-- -- |
-- -- Traverses the type constructor names deeply up until it reaches the ones,
-- -- which have instances, or primitive types.
-- accumulateDictionaryRecords :: Name -> StateT [InstanceTpl.DictionaryRecord] Q ()
-- accumulateDictionaryRecords n = do
--   TyConI d -> case d of
--     DataD _ _ vars cons _ -> do
--       mapM_ visitVar vars
--       mapM_ visitCon cons
--     NewtypeD _ _ vars con _ -> do
--       mapM_ visitVar vars
--       visitCon con
--     d -> $bug $ "Unexpected declaration: " <> show d
--   $notImplemented

-- renderInheritedDictionary :: Name -> Q Exp
-- renderInheritedDictionary n = do
--   referredTypes <- do
--     cons <- reify n |$> \case
--       TyConI d -> case d of
--         DataD _ _ _ cons _ -> cons
--         NewtypeD _ _ _ con _ -> [con]
--         d -> $bug $ "Unexpected dec: " <> show d
--       PrimTyConI _ _ _ -> []
--     return $ join $ map conTypes $ cons
--   inhDics <- forM referredTypes $ \t -> 
--     [e| (snd . Class.graph) (undefined :: $(return t)) |]
--   [e| mconcat $(return $ ListE inhDics) |]
