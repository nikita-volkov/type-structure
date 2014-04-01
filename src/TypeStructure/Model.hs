module TypeStructure.Model where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data


-- |
-- A representation of the type structure graph.
-- 
-- Consists of a type signature of the subject type and 
-- a dictionary of all the types it refers to internally with
-- primitive types as end-nodes.
type Graph = (Type, Dictionary)

-- | A type signature.
data Type =
  Type_App Type Type |
  Type_Var TypeVar |
  Type_Con TypeCon
  deriving (Read, Show, Ord, Eq, Data, Typeable, Generic)

instance Hashable Type

-- | A type variable.
type TypeVar = Name

-- | A type constructor.
type TypeCon = (Namespace, Name)

-- | A name.
type Name = Text

-- | A namespace (module name).
type Namespace = Text

-- | A dictionary of type declarations indexed by type constructors.
type Dictionary = HashMap TypeCon Declaration

-- | A type declaration.
data Declaration = 
  Declaration_ADT [TypeVar] [Constructor] | 
  Declaration_Primitive |
  Declaration_Synonym [TypeVar] Type
  deriving (Read, Show, Ord, Eq, Data, Typeable, Generic)

instance Hashable Declaration

-- | A data constructor used in type declaration.
type Constructor = (Name, [Type])

