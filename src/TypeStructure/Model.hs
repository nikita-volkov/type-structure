module TypeStructure.Model where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data


type Graph = (Type, Dictionary)

-- | 
-- A type signature.
data Type =
  Type_App Type Type |
  Type_Var TypeVar |
  Type_Con TypeCon
  deriving (Show, Ord, Eq)

type TypeVar = Name

type TypeCon = (Namespace, Name)

type Name = Text

type Namespace = Text

type Dictionary = HashMap TypeCon Declaration

data Declaration = 
  Declaration_ADT [TypeVar] [Constructor] | 
  Declaration_Primitive |
  Declaration_Synonym [TypeVar] Type
  deriving (Show, Ord, Eq)

type Constructor = (Name, [Type])

