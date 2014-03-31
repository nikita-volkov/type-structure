module TypeStructure.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
  isInstance',
)
where

import TypeStructure.Prelude.Basic
import Language.Haskell.TH as Exports


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

isInstance' :: Name -> [Type] -> Q Bool
isInstance' name types = recover (return False) (isInstance name types)

