module TypeStructure.Prelude.Data
(
  module Exports,

  LazyByteString,
  LazyText,
  packText,
  unpackText,
)
where

-- hashable
import Data.Hashable as Exports (Hashable(..), hash)

-- bytestring
import Data.ByteString as Exports (ByteString)

-- text
import Data.Text as Exports (Text)

-- containers
import Data.Map as Exports (Map)
import Data.IntMap as Exports (IntMap)
import Data.Set as Exports (Set)
import Data.IntSet as Exports (IntSet)
import Data.Sequence as Exports (Seq)
import Data.Tree as Exports (Tree)

-- unordered-containers
import Data.HashMap.Strict as Exports (HashMap)
import Data.HashSet as Exports (HashSet)

-- array
import Data.Array as Exports (Array)
import Data.Array.Unboxed as Exports (UArray)
import Data.Array.IArray as Exports (IArray)

-- vector
import Data.Vector as Exports (Vector)

-- time
import Data.Time as Exports (Day, DiffTime, NominalDiffTime, UniversalTime, UTCTime, LocalTime, TimeOfDay, TimeZone, ZonedTime)
import Data.Time.Clock.TAI as Exports (AbsoluteTime)


import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text

type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

packText = Data.Text.pack
unpackText = Data.Text.unpack
