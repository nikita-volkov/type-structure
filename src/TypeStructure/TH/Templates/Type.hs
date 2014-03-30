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
module TypeStructure.TH.Templates.Type where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import TypeStructure.Prelude.TH
import qualified TypeStructure.Graph as G
import qualified TypeStructure.Class as C
import qualified TypeStructure.TH.Templates.TypeCon as TypeCon

render :: Settings -> Exp
render = \case
  App l r -> purify [e| G.Type_App $(return $ render $ l) $(return $ render $ r) |]
  Var n -> purify [e| G.Type_Var $(stringE $ n) |]
  Con tcs -> purify [e| G.Type_Con $(return $ TypeCon.render tcs) |]

data Settings =
  App Settings Settings |
  Var String |
  Con TypeCon.Settings
