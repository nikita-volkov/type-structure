
module TypeStructure.TH.Template where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.TH.Model
import qualified TypeStructure.Prelude.TH as T
import qualified TypeStructure.Model as M
import qualified TypeStructure.Class as C

-- |
-- 1.
-- @
-- Type_Var "a"
-- @
-- 
-- 2.
-- @
-- Type_App (Type_Con ("GHC.Types", "[]")) (Type_Var "a")])
-- @
-- 
renderType :: Type -> T.Exp
renderType = \case
  App l r -> T.purify [e| M.Type_App $(return $ renderType $ l) $(return $ renderType $ r) |]
  Var n -> T.purify [e| M.Type_Var $(T.stringE $ n) |]
  Con tcs -> T.purify [e| M.Type_Con $(return $ renderTypeCon tcs) |]

-- |
renderTypeCon :: TypeCon -> T.Exp
renderTypeCon (ns, n) = T.purify [e| ($(T.stringE ns), $(T.stringE n)) |]

-- |
-- Either
-- 
-- @
-- let
--   vars = ["a"]
--   cons =
--     [("[]", []),
--      (":", 
--       [Type_Var "a",
--        Type_App (Type_Con ("GHC.Types", "[]")) (Type_Var "a")])]
--   in Dec_ADT vars cons
-- @
-- 
-- or
-- 
-- @
-- Dec_Primitive
-- @
-- 
renderDeclaration :: Declaration -> T.Exp
renderDeclaration = \case
  Primitive _ -> T.purify [e| M.Declaration_Primitive |]
  ADT vars constructors -> T.purify [e| M.Declaration_ADT $varsE $consE |] where
    varsE = T.listE $ map T.stringE vars
    consE = T.listE $ map renderCons constructors
      where
        renderCons (n, tss) = [e| ($nameE, $typesE) |] where
          nameE = T.stringE $ n
          typesE = T.listE $ map (return . renderType) tss
  Synonym vars t -> T.purify [e| M.Declaration_Synonym $varsE $typeE |] where
    varsE = T.listE $ map T.stringE vars
    typeE = return $ renderType t

-- |
-- @
-- instance TypeStructure a => TypeStructure (GHC.Types.[] a) where
--   graph = const $ (finalType, dictionary)
--     where
--       finalType = Type_App (Type_Con ("GHC.Types", "[]")) ((fst . graph) (undefined :: a))
--       dictionary = Data.HashMap.Strict.insert typeCon declaration inheritedDictionary
--         where
--           typeCon = ("GHC.Types", "[]")
--           declaration = 
--             let
--               vars = ["a"]
--               cons =
--                 [("[]", []),
--                  (":", 
--                   [Type_Var "a",
--                    Type_App (Type_Con ("GHC.Types", "[]")) (Type_Var "a")])]
--               in Dec_ADT vars cons
--           inheritedDictionary = mconcat [(snd . graph) (undefined :: a)]
-- @
-- 
renderInstance
  :: T.Name -- ^ Type con 
  -> [TypeVar]
  -> TypeCon
  -> [(TypeCon, Declaration)] -- ^ Records
  -> [T.Type] -- ^ TH Types to inherit dictionaries from
  -> T.Dec
renderInstance name vars tcs dictionaryRecords inheritedDictionaries =
  T.InstanceD 
    context 
    (T.ConT ''C.TypeStructure `T.AppT` headType) 
    [graphDec]
  where
    context = flip map varTypes $ (T.ClassP ''C.TypeStructure . pure)
    varTypes = map (T.VarT . T.mkName) vars
    headType = foldl T.AppT (T.ConT name) varTypes
    graphDec = T.FunD 'C.graph [T.Clause [] (T.NormalB exp) [finalTypeDec, dictionaryDec, typeConDec]] 
      where
        exp = T.purify $ [e| const ($(T.varE $ T.mkName "finalType"), $(T.varE $ T.mkName "dictionary")) |]
        finalTypeDec = T.FunD (T.mkName "finalType") [clause] where
          clause = T.Clause [] (T.NormalB exp) [] where
            exp = 
              T.purify $
              foldM 
                (\l r -> [e| M.Type_App $(return l) $(return r) |]) 
                (T.purify [e| M.Type_Con $(T.varE $ T.mkName "typeCon") |])
                (map varExp varTypes)
              where
                varExp t = T.purify [e| ((fst . C.graph) (undefined :: $(return t))) |]
        dictionaryDec = T.FunD (T.mkName "dictionary") [T.Clause [] (T.NormalB exp) []] where
          exp = T.purify
            [e|
              let
                newRecords = $(
                    return $ T.ListE $ flip map dictionaryRecords $ \(tcs, ds) ->
                      T.purify 
                        [e|
                          let 
                            typeCon = $(return $ renderTypeCon tcs)
                            declaration = $(return $ renderDeclaration ds)
                            in (typeCon, declaration)
                        |]
                  )
                inheritedDictionary = 
                  mconcat $ dicsOfTypeVars ++ dicsOfReferredTypes
                  where
                    dicsOfTypeVars = $(return $ T.ListE $ map dictionaryExp varTypes)
                    dicsOfReferredTypes = $(return $ T.ListE $ map dictionaryExp inheritedDictionaries)
                in foldr (\p -> (p:) . delete p) inheritedDictionary newRecords
            |]
            where
              dictionaryExp t = T.purify [e| ((snd . C.graph) (undefined :: $(return t))) |]
        typeConDec = T.FunD (T.mkName "typeCon") [T.Clause [] (T.NormalB exp) []] where
          exp = renderTypeCon tcs
