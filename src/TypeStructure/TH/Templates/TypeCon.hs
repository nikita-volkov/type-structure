-- |
module TypeStructure.TH.Templates.TypeCon where

import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.Data
import qualified TypeStructure.Prelude.TH as T
import qualified TypeStructure.Graph as G
import qualified TypeStructure.Class as C

render :: Settings -> T.Exp
render (ns, n) = T.purify [e| ($(T.stringE ns), $(T.stringE n)) |]

type Settings = (Namespace, Name)
type Namespace = String
type Name = String

