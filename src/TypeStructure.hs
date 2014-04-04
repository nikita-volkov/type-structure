-- |
-- This library provides facilities to get a representation data of a type, 
-- while traversing all the types it refers to down to the primitives. 
-- Thus it transitively captures all the types involved, 
-- which may even come from different libraries. 
-- 
-- The produced data structure is a graph, which has a 'Hashable' instance, 
-- so it can be used to perform the matching aswell. 
-- E.g., one can produce a \"version\" hash with it.
-- 
-- To provide support by this library for any type use the TemplateHaskell 
-- to 'derive' an instance of the 'TypeStructure' class like this:
-- 
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import qualified TypeStructure
-- > 
-- > data AnyData = A | B Int | C Char
-- > 
-- > TypeStructure.derive ''AnyData
-- 
-- Following is a GHCi session, showing, how the library is supposed to be used:
-- 
-- > λ>import TypeStructure 
-- 
-- Construction of the type structure representation graph: 
-- 
-- > λ>graph (undefined :: Int)
-- >
-- > ( 
-- >   Type_Con ("GHC.Types","Int"),
-- >   [
-- >     ( 
-- >       ("GHC.Types","Int"),
-- >       Declaration_ADT [] 
-- >         [
-- >           ("I#",[Type_Con ("GHC.Prim","Int#")])
-- >         ] 
-- >     ),
-- >     (
-- >       ("GHC.Prim","Int#"),
-- >       Declaration_Primitive
-- >     )
-- >   ]
-- > )
-- 
-- Graphs of different types are guaranteed to be different:
-- 
-- > λ>graph (undefined :: Int) /= graph (undefined :: Integer)
-- >
-- > True
-- 
-- Graphs of values of the same type are guaranteed to be the same:
-- 
-- > λ>graph True == graph False
-- >
-- > True    
-- 
-- Acquiring a hash of the typestructure:
-- 
-- > λ>import Data.Hashable
-- >
-- > λ>hash $ graph (undefined :: Int)
-- >
-- > 3224108341943761557
-- 
-- Hashes of different types should not be equal:
-- 
-- > λ>(hash $ graph (undefined :: Int)) /= (hash $ graph (undefined :: Integer))
-- >
-- > True
-- 
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
derive ''E0
derive ''E1
derive ''E2
derive ''E3
derive ''E6
derive ''E9
derive ''E12
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
derive ''ThreadId
derive ''TypeRep
derive ''StableName

-- tuples
fmap join $ mapM derive $ map TH.tupleTypeName [2..24]

-- transformers
---------------------
derive ''Identity

-- vector
---------------------
derive ''Vector

-- array
---------------------
derive ''UArray
derive ''Array

-- containers
---------------------
derive ''IntSet
derive ''IntMap
derive ''Set
derive ''Tree
derive ''Map

-- unordered-containers
---------------------
derive ''HashSet
derive ''HashMap

-- bytestring
---------------------
derive ''ByteString
derive ''LazyByteString

-- text
---------------------
derive ''Text
derive ''LazyText

-- time
---------------------
derive ''AbsoluteTime
derive ''ZonedTime
derive ''LocalTime
derive ''TimeZone
derive ''TimeOfDay
derive ''NominalDiffTime
derive ''UTCTime
derive ''UniversalTime
derive ''DiffTime
derive ''Day

-- type-structure
---------------------
derive ''Type
derive ''Declaration
