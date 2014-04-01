module TypeStructure.Class where

import TypeStructure.Prelude.Basic
import TypeStructure.Model


-- |
-- A type structure graph production.
-- 
-- Supposed to be used like this:
-- 
-- @
-- graph (undefined :: Int)
-- @
class TypeStructure a where
  graph :: a -> Graph
