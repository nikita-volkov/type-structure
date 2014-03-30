module TypeStructure.TH where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import Language.Haskell.TH
import qualified TypeStructure.Graph as G
import qualified TypeStructure.Class as Class
import qualified Data.HashMap.Strict as HashMap
import qualified TypeStructure.TH.Templates.Instance as InstanceTpl
import qualified TypeStructure.TH.Templates.Declaration as DeclarationTpl
import qualified TypeStructure.TH.Templates.Type as TypeTpl
import qualified TypeStructure.TH.Templates.TypeCon as TypeConTpl

derive :: Name -> Q [Dec]
derive name = do
  -- vars <- reifyVars name
  -- let
  --   context = flip map vars $ (ClassP ''Class.TypeStructure . pure)
  --   head = foldl AppT (ConT name) vars
  -- graphDec <- do
  --   body <-
  --     [e|
  --       let
  --         finalType = $(renderFinalType name)
  --         dictionary = HashMap.insert typeCon declaration inheritedDictionary where
  --           typeCon = $(renderTypeCon name)
  --           declaration = $(renderDec name)
  --           inheritedDictionary = $(renderInheritedDictionary name)
  --         in const $ (finalType, dictionary)
  --     |]
  --   return $ FunD 'Class.graph [Clause [] (NormalB body) []]
  -- return $ pure $ 
  --   InstanceD 
  --     context 
  --     (ConT ''Class.TypeStructure `AppT` head) 
  --     [graphDec]
  $notImplemented
  where
    -- processName n = do
    --   (lift $ reify n) >>= \case
    --     TyConI d -> case d of
    --       DataD _ _ vars cons _ -> do
    --         mapM_ processVar vars
    -- processVar = \case
    --   PlainTV n -> $notImplemented
    --   k -> $bug $ "Unexpected kinded var: " <> show k






-- reifyVars :: Name -> Q [Type]
-- reifyVars name = do
--   reify name |$> \case
--     TyConI d -> case d of
--       DataD _ _ vars _ _ -> map varType vars
--       NewtypeD _ _ vars _ _ -> map varType vars
--       _ -> $notImplemented
--     PrimTyConI _ arity _ -> take arity ['a'..] |> map (VarT . mkName . pure)
--     _ -> $notImplemented
--   where
--     varType = \case
--       PlainTV n -> VarT n
--       v -> $bug $ "Unexpected var: " <> show v

-- renderFinalType :: Name -> Q Exp
-- renderFinalType n = do
--   varTypes <- reifyParams n
--   varExps <- forM varTypes $ \t ->
--     [e| ((fst . Class.graph) (undefined :: $(return t))) |]
--   conExp <- 
--     [e| G.Type_Con $(renderTypeCon n) |]
--   foldM (\l r -> [e| G.Type_App $(return l) $(return r) |]) conExp varExps

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

-- conTypes :: Con -> [Type]
-- conTypes = \case
--   NormalC n ts -> map snd ts
--   RecC n ts -> map (\(_, _, t) -> t) ts
--   InfixC (_, a) n (_, b) -> [a, b]
--   c -> $bug $ "Unexpected constructor: " <> show c

-- renderDec :: Name -> Q Exp
-- renderDec name = do
--   reify name >>= \case
--     TyConI dec -> case dec of
--       DataD _ _ vars cons _ -> fromVarsAndCons vars cons
--       NewtypeD _ _ vars con _ -> fromVarsAndCons vars [con]
--       -- TySynD _ vars t -> $notImplemented
--       _ -> $bug $ "Unexpected dec: " <> show dec
--       where
--         fromVarsAndCons vars cons = [e| G.Dec_ADT $varsE $consE |]
--           where 
--             varsE = do
--               exps <- forM vars $ \case
--                 PlainTV n -> stringE $ nameBase n
--                 KindedTV n k -> $bug $ "Unexpected kinded var: " <> show n
--               return $ ListE exps
--             consE = do
--               exps <- forM cons $ \case
--                 NormalC n ts -> do
--                   ts' <- forM ts $ \(_, t) -> renderType t
--                   [e| ($(stringE $ nameBase $ n), $(return $ ListE ts')) |]
--                 RecC n ts -> do
--                   ts' <- forM ts $ \(_, _, t) -> renderType t
--                   [e| ($(stringE $ nameBase $ n), $(return $ ListE ts')) |]
--                 InfixC (_, a) n (_, b) -> do
--                   [e| ($(stringE $ nameBase $ n), $(listE [renderType a, renderType b])) |]
--                 c -> $bug $ "Unexpected constructor: " <> show c
--               return $ ListE exps
--     PrimTyConI _ arity unlifted -> [e| G.Dec_Primitive |]
--     _ -> $bug $ "Not a valid name of a type: " <> show name

-- renderType :: Type -> Q Exp
-- renderType = \case
--   AppT l r -> [e| G.Type_App $(renderType l) $(renderType r) |]
--   VarT n -> [e| G.Type_Var $(stringE $ nameBase $ n) |]
--   ConT n -> fromName n
--   TupleT a -> fromName $ tupleTypeName a
--   UnboxedTupleT a -> fromName $ unboxedTupleTypeName a
--   ArrowT -> fromName ''(->)
--   ListT -> fromName ''[]
--   t -> $bug $ "Unsupported type: " <> show t
--   where
--     fromName n = [e| G.Type_Con $(renderTypeCon n) |]

-- renderTypeCon :: Name -> Q Exp
-- renderTypeCon n = do
--   mn <- maybe (fail "Name without namespace") return $ nameModule n
--   [e| ($(stringE mn), $(stringE $ nameBase $ n)) |]
