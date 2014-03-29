module TypeStructure.Graph where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data


type Graph = (Type, Dictionary)

-- | 
-- A type signature.
-- Can be used as an index.
data Type =
  Type_App Type Type |
  Type_Var Var |
  Type_Con TypeCon
  deriving (Show, Ord, Eq)

type Var = Name

-- -- |
-- -- 
-- data TypeCon = 
--   TypeCon_ID Namespace Name |
--   TypeCon_Tuple Arity |
--   TypeCon_Arrow |
--   TypeCon_List
--   deriving (Show, Ord, Eq)
type TypeCon = (Namespace, Name)

type Arity = Int

type Dictionary = HashMap TypeCon Dec

data Dec = 
  Dec_ADT [Var] [Constructor] | 
  Dec_Primitive 
  deriving (Show, Ord, Eq)

type Constructor = (Name, [Type])

type Name = Text

type Namespace = Text
