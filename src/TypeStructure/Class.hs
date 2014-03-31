module TypeStructure.Class where

import TypeStructure.Prelude.Basic
import TypeStructure.Model


-- |
-- A type structure graph production.
-- 
-- graph (undefined :: Artist)
class TypeStructure a where
  graph :: a -> Graph

-- -- |
-- -- A type structure graph production.
-- -- 
-- -- graph (structure :: Structure Artist)
-- class TypeStructure a where
--   data Structure a
--   structure :: Structure a
--   graph :: Structure a -> Graph

