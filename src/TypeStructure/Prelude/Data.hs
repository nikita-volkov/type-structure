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

-- text
import Data.Text as Exports (Text)

-- unordered-containers
import Data.HashMap.Strict as Exports (HashMap)


import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text

type LazyByteString = Data.ByteString.Lazy.ByteString
type LazyText = Data.Text.Lazy.Text

packText = Data.Text.pack
unpackText = Data.Text.unpack
