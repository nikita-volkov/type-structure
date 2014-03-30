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
module TypeStructure.TH.Templates.Instance where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.Prelude.TH
import qualified TypeStructure.Graph as G
import qualified TypeStructure.Class as C
import qualified TypeStructure.TH.Templates.Declaration as Declaration
import qualified TypeStructure.TH.Templates.TypeCon as TypeCon
import qualified TypeStructure.TH.Templates.Type as Type
import qualified Data.HashMap.Strict as HashMap

render :: Settings -> Dec
render (name, vars, tcs, ds, ndr, rthi) =
  InstanceD 
    context 
    (ConT ''C.TypeStructure `AppT` headType) 
    [graphDec]
  where
    context = flip map varTypes $ (ClassP ''C.TypeStructure . pure)
    varTypes = map VarT vars
    headType = foldl AppT (ConT name) varTypes
    graphDec = FunD 'C.graph [Clause [] (NormalB exp) [finalTypeDec, dictionaryDec, typeConDec]] 
      where
        exp = purify $ [e| const ($(varE $ mkName "finalType"), $(varE $ mkName "dictionary")) |]
        finalTypeDec = FunD (mkName "finalType") [clause] where
          clause = Clause [] (NormalB exp) [] where
            exp = 
              purify $
              foldM 
                (\l r -> [e| G.Type_App $(return l) $(return r) |]) 
                (purify [e| G.Type_Con $(varE $ mkName "typeCon") |])
                (map varExp varTypes)
              where
                varExp t = purify [e| ((fst . C.graph) (undefined :: $(return t))) |]
        dictionaryDec = FunD (mkName "dictionary") [Clause [] (NormalB exp) []] where
          exp = purify
            [e|
              let
                newRecords = currentRecord : recordsOfReferredTypes where
                  currentRecord = purify
                    $([e| 
                      let 
                        typeCon = $(varE $ mkName $ "typeCon")
                        declaration = $(return $ Declaration.render ds)
                        in (typeCon, declaration)
                    |])
                  recordsOfReferredTypes = 
                    $(
                      return $ ListE $ flip map ndr $ \(tcs, ds) ->
                        -- let 
                        --   tc = return $ TypeCon.render tcs
                        --   d = return $ Declaration.render ds
                        --   in purify $ [e| ($tc, $d) |]
                        purify 
                          [e|
                            let 
                              typeCon = $(return $ TypeCon.render tcs)
                              declaration = $(return $ Declaration.render ds)
                              in (typeCon, declaration)
                          |]
                    )
                inheritedDictionary = 
                  mconcat $ dicsOfTypeVars ++ dicsOfReferredTypes
                  where
                    dicsOfTypeVars = $(return $ ListE $ map dictionaryExp varTypes)
                    dicsOfReferredTypes = $(return $ ListE $ map dictionaryExp rthi)
                in foldr (\(k, v) -> HashMap.insert k v) inheritedDictionary newRecords
            |]
            where
              dictionaryExp t = purify [e| ((snd . C.graph) (undefined :: $(return t))) |]
        typeConDec = FunD (mkName "typeCon") [Clause [] (NormalB exp) []] where
          exp = TypeCon.render tcs

type Settings = 
  (Name, [TypeVar], TypeCon.Settings, Declaration.Settings, [InlinedDeclaration], [InheritedDictionary])

type TypeVar = Name

type InlinedDeclaration = (TypeCon.Settings, Declaration.Settings)

type InheritedDictionary = Type
