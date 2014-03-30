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
module TypeStructure.TH.Templates.Declaration where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.Prelude.TH
import qualified TypeStructure.Graph as G
import qualified TypeStructure.Class as C
import qualified TypeStructure.TH.Templates.Type as Type

render :: Settings -> Exp
render = \case
  Primitive -> purify [e| G.Dec_Primitive |]
  ADT vars constructors -> purify [e| G.Dec_ADT $varsE $consE |] where
    varsE = listE $ map (stringE . nameBase) vars
    consE = listE $ map renderCons constructors
      where
        renderCons (n, tss) = [e| ($nameE, $typesE) |] where
          nameE = stringE $ nameBase $ n
          typesE = listE $ map (return . Type.render) tss

data Settings = 
  Primitive |
  ADT [Var] [Constructor]

type Var = Name

type Constructor = (Name, [Type.Settings])

