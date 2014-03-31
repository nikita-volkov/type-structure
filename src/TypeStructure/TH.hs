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
  (inlinedRecords, dictionaries) <- partitionEithers <$> execWriterT (accumulateDictionaries name)
  return $ (:[]) $ Template.renderInstance 
    name vars typeCon (nub $ (typeCon, declaration) : inlinedRecords) (nub dictionaries)
  where
    deriveTypeCon name = do
      ns <- maybe (fail "Name without namespace") return $ nameModule name
      return (ns, nameBase name)
    accumulateDictionaries name = do
      info <- lift $ reify $ name
      forM_ (A.referredTypes info) $ \t -> do
        (lift $ isInstance' ''Class.TypeStructure [t]) >>= \case
          True -> tell $ pure $ Right $ t
          False -> case t of
            ConT n -> do
              info <- lift $ reify n
              let 
                typeCon = 
                  A.adaptTypeConName n ?: 
                  ($bug $ "Name without namespace: " <> show n)
                declaration = A.infoToDeclaration info
                in tell $ pure $ Left $ (typeCon, declaration)
              accumulateDictionaries n
            _ -> do
              let 
                traverseType = \case
                  AppT l r -> traverseType l >> traverseType r
                  VarT n -> return ()
                  ConT n -> accumulateDictionaries n
                  _ -> return ()
                in traverseType t
