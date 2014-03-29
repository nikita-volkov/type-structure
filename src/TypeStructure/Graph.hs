module TypeStructure.Graph where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data


-- type Type = (TypeID, TypeSpec)
type Graph = (TypeID, TypesMap)
type TypeID = (Namespace, Name)
type TypesMap = HashMap TypeID TypeSpec
data TypeSpec = ADT [Constructor] | Primitive deriving (Show)
type Constructor = (Name, [TypeID])
type Name = Text
type Namespace = Text
