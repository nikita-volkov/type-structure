module TypeStructure
(
  -- * Class
  TypeStructure(..), 
  derive,
  -- * Model
  module TypeStructure.Model, 
)
where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data
import TypeStructure.Model
import TypeStructure.Class
import TypeStructure.TH
import qualified TypeStructure.Prelude.TH as TH


derive ''()
derive ''[]
derive ''Int
derive ''Text
fmap join $ mapM derive $ map TH.tupleTypeName [2..7]
