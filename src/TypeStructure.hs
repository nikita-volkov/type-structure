module TypeStructure
(
  module TypeStructure.Graph, 
  TypeStructure(..), 
  derive,
)
where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data
import TypeStructure.Graph
import TypeStructure.Class
import TypeStructure.TH

-- $(
--   fmap join $ mapM derive $
--     [
--       ''Text
--     ]
--   )
