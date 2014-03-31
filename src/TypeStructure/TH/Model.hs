module TypeStructure.TH.Model where

import TypeStructure.Prelude.Basic

-- NOTE!
-- Oddly enough, this model almost completely replicates the output Model itself, 
-- so... Refactoring, baby!

data Type =
  App Type Type |
  Var TypeVar |
  Con TypeCon
  deriving (Show, Eq, Ord)

type TypeCon = (Namespace, Name)

type Namespace = String

type Name = String

type TypeVar = Name

data Declaration = 
  Primitive Arity |
  ADT [TypeVar] [Constructor] |
  Synonym [TypeVar] Type
  deriving (Show, Eq, Ord)

type Arity = Int

type Constructor = (Name, [Type])

