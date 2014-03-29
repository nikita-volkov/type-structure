module TypeStructure.TH where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import Language.Haskell.TH
import qualified TypeStructure.Graph as Graph
import qualified TypeStructure.Class as Class
import qualified Data.HashMap.Strict as HashMap



derive :: Name -> Q [Dec]
derive name = do
  typeID <- maybe (fail "Name without namespace") return $ typeIDFromName name
  $notImplemented

typeIDFromName :: Name -> Maybe Graph.TypeID
typeIDFromName n = (,) <$> (packText <$> nameModule n) <*> (pure $ packText $ nameBase n)

-- processConstructor :: Bool -> Con -> Q Graph.Constructor
-- processConstructor deep = \case
--   NormalC n sts -> $notImplemented


-- processType = \case
--   ConT n -> reifyTypeSpec n

-- reifyTypeSpec :: Name -> Q Graph.TypeSpec
-- reifyTypeSpec nm = do
--   reify nm >>= \case
--     TyConI dec -> case dec of
--       DataD _ name vars cons _ -> $notImplemented
--     PrimTyConI _ arity unlifted ->
--       return $ Graph.Primitive
--     _ -> fail $ "Unsupported type of name: " <> show nm

analyze :: Name -> Q Graph.Graph
analyze n = runStateT (aggregateTypesMap n) HashMap.empty

aggregateTypesMap :: Name -> StateT (HashMap Graph.TypeID Graph.TypeSpec) Q Graph.TypeID
aggregateTypesMap = \n -> do
  tid <- lift $ maybe (fail "Name without namespace") return $ typeIDFromName n
  get |$> HashMap.lookup tid >>= \case
    Just _ -> return ()
    Nothing -> do
      (lift $ reify n) >>= \case
        TyConI dec -> case dec of
          DataD _ _ vars cons _ -> $notImplemented
        PrimTyConI _ arity unlifted -> do
          modify $ HashMap.insert tid Graph.Primitive
        _ -> lift $ fail $ "Unsupported type of name: " <> show n
  return tid
  where
    processConstructor = \case
      NormalC n sts -> do
        tids <- mapM processStrictType sts
        return (packText $ nameBase $ n, tids)
      _ -> $notImplemented
    processStrictType (_, t) = processType t
    processType = \case
      ConT n -> aggregateTypesMap n

