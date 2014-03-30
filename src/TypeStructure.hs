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
import qualified TypeStructure.Prelude.TH as TH


$(
  fmap join $ mapM derive $
    [
      ''[],
      ''Int, 
      ''Text, 
      ''(->),
      ''()
    ] ++
    map TH.tupleTypeName [2..7]
  )

