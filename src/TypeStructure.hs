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
import TypeStructure.Prelude.Transformers
import TypeStructure.Model
import TypeStructure.Class
import TypeStructure.TH
import qualified TypeStructure.Prelude.TH as TH
import qualified GHC.Exts



-- base
---------------------
derive ''(->)
derive ''()
derive ''[]
derive ''Int
derive ''Int8
derive ''Int16
derive ''Int32
derive ''Int64
derive ''Integer
derive ''Word
derive ''Word8
derive ''Word16
derive ''Word32
derive ''Word64
derive ''Float
derive ''Double
derive ''Char
derive ''Bool
derive ''Ordering
derive ''Fixed
derive ''Ratio
derive ''Last
derive ''First
derive ''Any
derive ''All
derive ''Sum
derive ''Product
derive ''Dual
derive ''Seq
derive ''Maybe
derive ''Either
derive ''GHC.Exts.Any

-- tuples
fmap join $ mapM derive $ map TH.tupleTypeName [2..24]

-- -- transformers
-- ---------------------
-- derive ''Identity

-- -- vector
-- ---------------------
-- derive ''Vector

-- -- array
-- ---------------------
-- derive ''UArray
-- derive ''Array

-- -- containers
-- ---------------------
-- derive ''IntSet
-- derive ''IntMap
-- derive ''Set
-- derive ''Tree
-- derive ''Map

-- -- unordered-containers
-- ---------------------
-- derive ''HashSet
-- derive ''HashMap

-- -- bytestring
-- ---------------------
-- -- derive ''ByteString

-- -- text
-- ---------------------
-- derive ''Text

-- -- time
-- ---------------------
-- derive ''AbsoluteTime
-- derive ''ZonedTime
-- derive ''LocalTime
-- derive ''TimeZone
-- derive ''TimeOfDay
-- derive ''NominalDiffTime
-- derive ''UTCTime
-- derive ''UniversalTime
-- derive ''DiffTime
-- derive ''Day
